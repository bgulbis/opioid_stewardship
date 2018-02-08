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
    mutate_at("service", str_replace_all, pattern = c("Cardiology Service" = "Cardiology",
                                                      "Obstetrics" = "Ob/Gyn Service")) %>%
    tidy_data() %>%
    left_join(ids, by = "pie.id") 

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("service.dc" = "Medical Service (Curr)"))


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
    distinct(millennium.id, event.id, med.datetime, .keep_all = TRUE)

meds_services <- meds_opiods %>%
    left_join(services, by = "millennium.id") %>%
    filter(med.datetime >= start.datetime,
           med.datetime < end.datetime) 

# miss_meds <- meds_opiods %>%
#     anti_join(meds_services, by = "event.id") %>%
#     left_join(demog[c("millennium.id", "service.dc")], by = "millennium.id")

data_meds_cont <- meds_services %>%
    filter(!is.na(event.tag)) %>%
    calc_runtime() %>%
    summarize_data() 
# add service

df_meds <- meds_services %>%
    filter(is.na(event.tag)) %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

mbo_orders <- concat_encounters(df_meds$orig.order.id)

# run MBO query
#   * 07_orders-details_opiods

orders_opiods <- read_data(dir_raw, "orders", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           order.id = `Order Id`,
           med.product = `Mnemonic (Product)`,
           route = `Order Route`,
           frequency = Frequency,
           prn = `PRN Indicator`) %>%
    select(-route) %>%
    distinct() 

data_meds <- df_meds %>%
    left_join(orders_opiods, by = c("millennium.id", "orig.order.id" = "order.id"))

# extract med strength / concentration


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
pain_pca <- read_data(dir_raw, "pca", FALSE) %>%
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
    mutate(total_dose = sum(pca_delivered * pca_dose, pca_load, na.rm = TRUE)) %>%
    left_join(services, by = "millennium.id") %>%
    filter(event.datetime >= start.datetime,
           event.datetime < end.datetime) 

data_pca <- pain_pca %>%
    group_by(millennium.id, pca_drug) %>%
    summarize_at(c("pca_demands",
                   "pca_delivered",
                   "total_dose"),
                 sum, na.rm = TRUE)
