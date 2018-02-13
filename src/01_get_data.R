library(tidyverse)
library(lubridate)
library(edwr)

tz <- "US/Central"
data_month <- mdy("1/1/2017", tz = tz)
month_abbrv <- format(data_month, "%Y-%m")

dir_raw <- paste0("data/raw/", month_abbrv)

dirr::gzip_files(dir_raw)

opiods <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    distinct(med.name)

opiods_lower <- mutate_at(opiods, "med.name", str_to_lower) 

opiods_list <- sort(unique(c(opiods$med.name, opiods_lower$med.name, "FENTanyl", "OXYcodone")))

mbo_opiods <- concat_encounters(opiods_list)

# MBO queries in Opiod Stewardship folder

# run MBO query
#   * 01_patients_opiods
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics;HC Childrens

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(discharge.datetime >= mdy("1/1/2017", tz = tz))

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO query
#   * 02_meds-inpt_opiods
#   * 03_pca_opiods
#   * 04_demographics_opiods

# run EDW query
#   * 05_identifiers_opiods

ids <- read_data(dir_raw, "identifiers") %>%
    rename(millennium.id = `Millennium Encounter ID`,
           pie.id = `PowerInsight Encounter Id`) %>%
    distinct()
    # as.id()

edw_id <- concat_encounters(ids$pie.id)

# service lines ----------------------------------------

# run EDW query
#   * Service History

services <- read_data(dir_raw, "services") %>%
    as.services() %>%
    left_join(ids, by = "pie.id") %>%
    arrange(pie.id, start.datetime)

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("service.dc" = "Medical Service (Curr)"))

missing_service <- anti_join(ids, services, by = "pie.id") %>%
    left_join(demog, by = "millennium.id") 

# pain meds --------------------------------------------

# opiods <- tibble(name = c("narcotic analgesics",
#                           "narcotic analgesic combinations"),
#                  type = c(rep("class", 2), "med"),
#                  group = "sched")
# 
# cont_opiods <- tibble(name = c("fentanyl",
#                                "morphine",
#                                "hydromorphone",
#                                "mepiridine",
#                                "remifentanyl",
#                                "sufentanyl"),
#                       type = "med",
#                       group = "cont")

# 129229777

meds_opiods <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(floor_date(med.datetime, "month") == data_month) %>%
    mutate(
        route.group = case_when(
            route %in% c("DHT", "GT", "JT", "NG", "NJ", "OGT", "PEG", "PO", "PR") ~ "PO",
            route %in% c("IM", "IV", "IV Central", "IVP", "IVPB", "EPIDURAL", "DIALYSIS") ~ "IV",
            route %in% c("TOP", "Transdermal") ~ "TOP",
            route == "NASAL" ~ "NASAL"),
        orig.order.id = order.parent.id    
    ) %>%
    mutate_at("orig.order.id", na_if, y = 0) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

df_meds <- meds_opiods %>%
    filter(is.na(event.tag)) 

mbo_orders <- concat_encounters(df_meds$orig.order.id)

# run MBO query
#   * 07_orders-details_opiods

ed_units <- c("HH VUHH", "HH EDHH", "HH EDTR", "HH EREV", "HH OBEC", "HC EDPD")
icu_units <- c("HH CCU", "HH CVICU", "HH HFIC", "HH MICU",
               "HH STIC", "HH 7J", "HH NVIC", "HH TSIC")
imu_units <- c("HVI CIMU", "HH CVIMU", "HH HFIM", "HH MIMU",
               "HH SIMU", "HH 3CIM", "HH NIMU", "HH STRK")
floor_units <- c("HH 3JP", "HH 3CP", "HH 4WCP", "HH ACE", "HH 5ECP", "HH 5JP",
                 "HH 5WCP", "HH 6EJP", "HH 6WJP", "HH 8NJP", "HH EMU", "HH NEU",
                 "HH 8WJP", "HH 9EJP", "HH 9WJP", "HH REHA", "HH TCF")
womens_units <- c("HH WC5", "HH WC6N", "HH WCAN")
neonatal_units <- c("HC A8N4", "HC A8NH", "HC NICE", "HC NICW")
pedi_units <- c("HC A8OH", "HC A8PH", "HC CCN", "HC CSC", "HC PEMU", "HC PICU")

meds_services <- meds_opiods %>%
    left_join(services, by = "millennium.id") %>%
    filter(med.datetime >= start.datetime,
           med.datetime <= end.datetime) %>%
    mutate(location.group = case_when(med.location %in% ed_units ~ "ed",
                                      med.location %in% icu_units ~ "icu",
                                      med.location %in% imu_units ~ "imu",
                                      med.location %in% floor_units ~ "floor",
                                      med.location %in% womens_units ~ "womens",
                                      med.location %in% neonatal_units ~ "neo",
                                      med.location %in% pedi_units ~ "pedi",
                                      TRUE ~ "misc"))

orders_opiods <- read_data(dir_raw, "orders", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           order.id = `Order Id`,
           med.product = `Mnemonic (Product)`,
           route = `Order Route`,
           frequency = Frequency,
           prn = `PRN Indicator`) %>%
    select(-route) %>%
    distinct() 

y <- data_meds %>% filter(med.dose.units == "mL") %>% distinct(med.product)

data_meds <- meds_services %>%
    filter(is.na(event.tag),
           med.dose != 9999999) %>%
    left_join(orders_opiods, by = c("millennium.id", "orig.order.id" = "order.id")) %>%
    mutate_at("med.product", str_to_lower) %>%
    mutate(
        tab.mg = case_when(
            str_detect(med.product, "7.5-200mg") ~ 7.5,
            str_detect(med.product, "325-10mg") ~ 10,
            str_detect(med.product, "325-7.5") ~ 7.5,
            str_detect(med.product, "325(mg)?-5") ~ 5,
            str_detect(med.product, "300-30") ~ 30,
            str_detect(med.product, "300-60") ~ 60,
            str_detect(med.product, "50 mg") ~ 50,
            str_detect(med.product, "325 mg -7.5 mg/15ml") ~ 0.5,
            str_detect(med.product, "5 ml 120-12 mg/5 ml") ~ 2.4,
            str_detect(med.product, "10 mg/ml") ~ 10,
            str_detect(med.product, "2.5 mg/2.5 ml") ~ 1,
            str_detect(med.product, "16.2-30") ~ 30,
            str_detect(med.product, "12 microgram/hr patch") ~ 12,
            str_detect(med.product, "25 microgram/hr patch") ~ 25,
            str_detect(med.product, "50 microgram/hr patch") ~ 50,
            str_detect(med.product, "75 microgram/hr patch") ~ 75,
            str_detect(med.product, "100 microgram/hr patch") ~ 100),
        dose.mg = case_when(
            med.dose.units %in% c("tab", "mL", "supp", "patch") ~ tab.mg * med.dose,
            TRUE ~ med.dose),
        mme.iv = case_when(
            med == "buprenorphine" & route.group == "TOP" ~ 3.8,
            med == "buprenorphine" & route.group == "PO" ~ 3,
            med == "butorphenol" ~ dose.mg * 5, 
            med == "codeine" & route.group == "PO" ~ dose.mg * 0.1, 
            med == "fentanyl" & route.group == "IV" ~ dose.mg * 0.1,
            med == "fentanyl" & route.group == "NASAL" ~ dose.mg * 0.16 * 0.3,
            med == "fentanyl" & route.group == "TOP" ~ dose.mg * 7.2 * 0.3,
            med == "hydrocodone" ~ dose.mg * 1 * 0.3,
            med == "hydromorphone" & route.group == "PO" ~ dose.mg * 1.3,
            med == "hydromorphone" & route.group == "IV" ~ dose.mg * 6.7, 
            med == "levorphanol" ~ dose.mg * 5, 
            med == "mepridine" ~ dose.mg * 0.1,
            med == "methadone" & route.group == "PO" ~ dose.mg * 3 * 0.3,
            med == "morphine" & route.group == "PO" ~ dose.mg * 0.3,
            med == "nalbuphine" ~ dose.mg * 1,
            med == "opium" ~ dose.mg * 1 * 0.3,
            med == "oxycodone" & route.group == "PO" ~ dose.mg * 0.5, 
            med == "oxymorphone" & route.group == "PO" ~ dose.mg * 1,
            med == "oxymorphone" & route.group == "IV" ~ dose.mg * 10, 
            med == "pentazocine" ~ dose.mg * 0.37 * 0.3,
            med == "tapentadol" ~ dose.mg * 0.1, 
            med == "remifentanil" & med.dose.units == "mg" ~ dose.mg * 100, # dose recorded in mg; assume equiv with fentanyl
            med == "sufentanil" ~ dose.mg * 0.5, # 100mcg = 50mg
            med == "tramdaol" ~ dose.mg * 0.1 * 0.3,
            TRUE ~ dose.mg)
        )

z <- filter(data_meds, is.na(dose.mg))

# sources:
# Lexicomp - Opiod Agonist Conversion (from drug, to Morphine IM)
# https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovContra/Downloads/Opioid-Morphine-EQ-Conversion-Factors-March-2015.pdf
#   * note, doses listed in oral mme, converted to iv mme by * 0.3
# http://clincalc.com/opioids/ - sufentanil



# extract med strength / concentration

# x %>%
#     # filter(med %in% c("remifentanil", "sufentanil", "meperidine")) %>%
#     filter(!(med %in% c("acetaminophen-hydrocodone", "fentanyl", 
#                         "hydromorphone", "oxycodone", "tramadol",
#                         "methadone", "acetaminophen-codeine", "acetaminophen-oxycodone"))) %>%
#     ggplot(aes(x = location.group)) +
#     geom_bar() +
#     facet_wrap(~ med) +
#     coord_flip() +
#     themebg::theme_bg()


# miss_meds <- meds_opiods %>%
#     anti_join(meds_services, by = "event.id") %>%
#     left_join(demog[c("millennium.id", "service.dc")], by = "millennium.id")

# y <- miss_meds %>%
#     inner_join(services, by = "millennium.id") %>%
#     select(millennium.id, event.id, med, med.datetime, start.datetime:service.from)

data_meds_cont <- meds_opiods %>%
    filter(!is.na(event.tag))
    calc_runtime() %>%
    summarize_data() 
    
# pca --------------------------------------------------

pca_actions <- c("pca continuous rate dose" = "pca_rate",
                 "pca demand dose unit" = "pca_dose_unit",
                 "pca demand dose" = "pca_dose",
                 "pca doses delivered" = "pca_delivered",
                 "pca drug" = "pca_drug",
                 "pca loading dose" = "pca_load",
                 "pca lockout interval \\(minutes\\)" = "pca_lockout",
                 "pca total demands" = "pca_demands")

# filter for current month
pain_pca <- read_data(dir_raw, "pain-pca", FALSE) %>%
    as.pain_scores() %>%
    select(millennium.id:event.result) %>%
    mutate_at("event", str_replace_all, pattern = pca_actions) %>%
    distinct(millennium.id, event.datetime, event, .keep_all = TRUE) %>%
    spread(event, event.result) %>%
    mutate_at(c("pca_demands",
                "pca_dose",
                "pca_delivered",
                "pca_load",
                "pca_lockout",
                "pca_rate"),
              as.numeric) %>%
    group_by(millennium.id, event.datetime) %>%
    mutate(total_dose = sum(pca_delivered * pca_dose, pca_load, na.rm = TRUE))

data_pca <- pain_pca %>%
    group_by(millennium.id, pca_drug) %>%
    summarize_at(c("pca_demands",
                   "pca_delivered",
                   "total_dose"),
                 sum, na.rm = TRUE)
