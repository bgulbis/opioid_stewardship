library(tidyverse)
library(lubridate)
library(edwr)

tz <- "US/Central"
data_month <- mdy("1/1/2017", tz = tz)
month_abbrv <- format(data_month, "%Y-%m")
depart_max <- floor_date(data_month + months(1), "month")

dir_raw <- paste0("data/raw/", month_abbrv)

dirr::gzip_files(dir_raw)

opiods <- med_lookup(
    c(
        "narcotic analgesics", 
        "narcotic analgesic combinations"
    )
) %>%
    distinct(med.name)

opiods_lower <- mutate_at(opiods, "med.name", str_to_lower) 

opiods_list <- sort(
    unique(
        c(
            opiods$med.name, 
            opiods_lower$med.name, 
            "FENTanyl", 
            "OXYcodone"
        )
    )
)

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
#   * 08_visits_opiods
#   * 09_attendings_opiods

# run EDW query
#   * 05_identifiers_opiods

ids <- read_data(dir_raw, "identifiers") %>%
    rename(
        millennium.id = `Millennium Encounter ID`,
        pie.id = `PowerInsight Encounter Id`
    ) %>%
    distinct()
    # as.id()

edw_id <- concat_encounters(ids$pie.id)

# ed visits --------------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("service.dc" = "Medical Service (Curr)"))

ed_id <- demog %>%
    filter(visit.type == "Emergency") %>%
    left_join(ids, by = "millennium.id")

mbo_ed <- concat_encounters(ed_id$millennium.id)
# edw_ed <- concat_encounters(ed_id$pie.id)

# run MBO query
#   * 10_discharge-rx_opiods

# los -------------------------------------------------

visits <- read_data(dir_raw, "visits", FALSE) %>%
    as.visits()

los_month <- demog %>%
    # select(millennium.id:length.stay) %>%
    left_join(
        visits, 
        by = c("millennium.id", "facility", "visit.type")
    ) %>%
    group_by(millennium.id) %>%
    mutate(
        arrive = if_else(
            arrival.datetime < data_month,
            data_month,
            arrival.datetime
        ),
        depart = if_else(
            floor_date(discharge.datetime, "month") > data_month,
            depart_max,
            discharge.datetime
        ),
        los.month = difftime(depart, arrive, units = "days")
    ) %>%
    mutate_at("los.month", as.numeric) 

# attendings -------------------------------------------

md <- read_data(dir_raw, "attending", FALSE) %>%
    as.md()

# service lines ----------------------------------------

# run EDW query
#   * Service History

services <- read_data(dir_raw, "services") %>%
    as.services() %>%
    tidy_data() %>%
    left_join(ids, by = "pie.id") %>%
    left_join(visits, by = "millennium.id") %>%
    mutate(wrong.start = start.datetime < arrival.datetime)

# x <- summarize_at(services, "wrong.start", sum, na.rm = TRUE)


# missing_service <- anti_join(ids, services, by = "pie.id") %>%
#     left_join(demog, by = "millennium.id") 

# pain meds --------------------------------------------

routes_po <- c(
    "DHT",
    "GT", 
    "JT", 
    "NG", 
    "NJ", 
    "OGT",
    "PEG",
    "PO", 
    "PR"
)

routes_iv <- c(
    "IM", 
    "IV", 
    "IV Central", 
    "IVP", 
    "IVPB", 
    "EPIDURAL",
    "DIALYSIS"
)

routes_top <- c("TOP", "Transdermal")

ed_units <- c(
    "HH VUHH", 
    "HH EDHH",
    "HH EDTR",
    "HH EREV", 
    "HH OBEC", 
    "HC EDPD"
)

icu_units <- c(
    "HH CCU", 
    "HH CVICU", 
    "HH HFIC", 
    "HH MICU",
    "HH STIC", 
    "HH 7J", 
    "HH NVIC", 
    "HH TSIC"
)

imu_units <- c(
    "HVI CIMU", 
    "HH CVIMU", 
    "HH HFIM", 
    "HH MIMU",
    "HH SIMU", 
    "HH 3CIM", 
    "HH NIMU", 
    "HH STRK"
)

floor_units <- c(
    "HH 3JP", 
    "HH 3CP", 
    "HH 4WCP", 
    "HH ACE", 
    "HH 5ECP", 
    "HH 5JP",
    "HH 5WCP",
    "HH 6EJP", 
    "HH 6WJP",
    "HH 8NJP", 
    "HH EMU", 
    "HH NEU",
    "HH 8WJP", 
    "HH 9EJP", 
    "HH 9WJP", 
    "HH REHA", 
    "HH TCF"
)

womens_units <- c("HH WC5", "HH WC6N", "HH WCAN")

neonatal_units <- c("HC A8N4", "HC A8NH", "HC NICE", "HC NICW")

pedi_units <- c(
    "HC A8OH", 
    "HC A8PH",
    "HC CCN",
    "HC CSC", 
    "HC PEMU",
    "HC PICU"
)

meds_opiods <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(floor_date(med.datetime, "month") == data_month) %>%
    mutate(
        route.group = case_when(
            route %in% routes_po ~ "PO",
            route %in% routes_iv ~ "IV",
            route %in% routes_top ~ "TOP",
            route == "NASAL" ~ "NASAL"
        ),
        orig.order.id = order.parent.id    
    ) %>%
    mutate_at("orig.order.id", na_if, y = 0) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id))) %>%
    mutate(
        location.group = case_when(
            med.location %in% ed_units ~ "ed",
            med.location %in% icu_units ~ "icu",
            med.location %in% imu_units ~ "imu",
            med.location %in% floor_units ~ "floor",
            med.location %in% womens_units ~ "womens",
            med.location %in% neonatal_units ~ "neo",
            med.location %in% pedi_units ~ "pedi",
            TRUE ~ "misc"
        )
    )

df_meds <- meds_opiods %>%
    filter(is.na(event.tag)) 

mbo_orders <- concat_encounters(df_meds$orig.order.id)

# run MBO query
#   * 07_orders-details_opiods

orders_opiods <- read_data(dir_raw, "orders", FALSE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        order.id = `Order Id`,
        med.product = `Mnemonic (Product)`,
        route = `Order Route`,
        frequency = Frequency,
        prn = `PRN Indicator`
    ) %>%
    select(-route) %>%
    distinct() 

meds_intermit <- meds_opiods %>%
    filter(
        is.na(event.tag),
        med.dose != 9999999
    ) %>%
    left_join(
        orders_opiods, 
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    mutate_at("med.product", str_to_lower) %>%
    calc_morph_eq()

meds_md <- meds_intermit %>%
    left_join(md, by = "millennium.id") %>%
    filter(
        is.na(attending) |
            (med.datetime >= md.start & med.datetime <= md.stop)
    )

mme_int <- function(x, type, ...) {
    group_by <- quos(...)
    
    x %>%
        group_by(!!!group_by) %>%
        summarize_at("mme.iv", sum, na.rm = TRUE) %>%
        mutate(type = type)
}

data_mme_int <- mme_int(
    meds_intermit, 
    "intermittent",
    millennium.id, 
    route.group
) 

data_mme_int_md <- mme_int(
    meds_md, 
    "md",
    millennium.id, 
    route.group, 
    attending
)

data_mme_int_location <- mme_int(
    meds_intermit, 
    "location",
    millennium.id, 
    route.group, 
    med.location
) 

data_mme_cont <- meds_opiods %>%
    filter(
        !is.na(event.tag),
        floor_date(med.datetime, "month") == data_month
    ) %>%
    calc_runtime() %>%
    summarize_data(data_meds_cont) %>%
    mutate(
        mme.iv = case_when(
            med == "fentanyl" ~ cum.dose * 0.1,
            med == "hydromorphone" ~ cum.dose * 6.7,
            TRUE ~ cum.dose
        )
    ) %>%
    group_by(millennium.id) %>%
    summarize_at("mme.iv", sum, na.rm = TRUE) %>%
    mutate(type = "continuous")
    
# pca --------------------------------------------------

pca_actions <- c(
    "pca continuous rate dose" = "pca.rate",
    "pca demand dose unit" = "pca.dose.unit",
    "pca demand dose" = "pca.dose",
    "pca doses delivered" = "pca.delivered",
    "pca drug" = "pca.drug",
    "pca loading dose" = "pca.load",
    "pca lockout interval \\(minutes\\)" = "pca.lockout",
    "pca total demands" = "pca.demands"
)

# filter for current month
pain_pca <- read_data(dir_raw, "pca", FALSE) %>%
    as.pain_scores() %>%
    select(millennium.id:event.result) %>%
    mutate_at("event", str_replace_all, pattern = pca_actions) %>%
    distinct(millennium.id, event.datetime, event, .keep_all = TRUE) %>%
    spread(event, event.result) %>%
    mutate_at(
        c(
            "pca.demands",
            "pca.dose",
            "pca.delivered",
            "pca.load",
            "pca.lockout",
            "pca.rate"
        ),
        as.numeric
    ) %>%
    group_by(millennium.id, event.datetime) %>%
    filter(
        !is.na(pca.dose),
        floor_date(event.datetime, "month") == data_month
    ) %>%
    mutate(
        total.dose = sum(
            pca.delivered * pca.dose, 
            pca.load, 
            na.rm = TRUE
        )
    )

data_mme_pca <- pain_pca %>%
    group_by(millennium.id) %>%
    fill(pca.drug) %>%
    group_by(millennium.id, pca.drug) %>%
    summarize_at(
        c(
            "pca.demands",
            "pca.delivered",
            "total.dose"
        ),
        sum, 
        na.rm = TRUE
    ) %>%
    mutate(
        mme.iv = case_when(
            str_detect(pca.drug, "Fentanyl") ~ total.dose * 0.1,
            str_detect(pca.drug, "Hydromorphone") ~ total.dose * 6.7,
            str_detect(pca.drug, "Morphine") ~ total.dose
        )
    ) %>%
    filter(!is.na(mme.iv)) %>%
    group_by(millennium.id) %>%
    summarize_at("mme.iv", sum, na.rm = TRUE) %>%
    mutate(type = "pca")

# discharge rx -----------------------------------------

rx <- read_data(dir_raw, "discharge", FALSE) %>%
    as.meds_home(
        extras = list(
            "order.id" = "Order Id",
            "med.product" = "Mnemonic (Ordered As Name)",
            "frequency" = "Frequency",
            "prn" = "PRN Indicator",
            "med.dose" = "Order Volume Dose",
            "med.dose.units" = "Order Volume Dose Unit",
            "order.provider" = "Ordering Provider LIMITS",
            "order.provider.posn" = "Ordering Provider Position LIMITS",
            "clin.display" = "Complete Clinical Display Line"
        )
    ) %>%
    filter(med %in% opiods_lower$med.name) %>%
    mutate(
        route = str_extract(clin.display, "PO"),
        route.group = case_when(
            route %in% routes_po ~ "PO",
            route %in% routes_iv ~ "IV",
            route %in% routes_top ~ "TOP",
            route == "NASAL" ~ "NASAL"
        ),
        num.tab = str_extract(clin.display, "# [0-9]*"),
        num.day = str_extract(clin.display, "X [0-9]*"),
        num.refill = str_extract(clin.display, "[0-9] Refill\\(s\\)"),
        dose.day = case_when(
            frequency == "Daily" ~ 1,
            frequency %in% c("Q6H", "QID") ~ 4,
            frequency %in% c("Q8H", "TID") ~ 3,
            frequency %in% c("Q12H", "BID") ~ 2,
            frequency %in% c("Q4H", "Q4-6H") ~ 6
        )
    ) %>%
    mutate_at(
        c("num.tab", "num.day", "num.refill"), 
        str_replace_all, 
        pattern = "(#|X) | Refill\\(s\\)", 
        replacement = ""
    ) %>%
    mutate_at(
        c("med.dose", "num.tab", "num.day", "num.refill"), 
        as.numeric
    ) 

data_rx_ed <- rx %>%
    calc_morph_eq() %>%
    mutate(
        mme.iv.day = mme.iv * dose.day,
        mme.iv.dispense = mme.iv * num.tab
    )

# save data --------------------------------------------

dirr::save_rds(paste0("data/tidy/", month_abbrv), pattern = "data_")

# data_mme <- data_mme_int %>%
#     bind_rows(data_mme_cont, data_mme_pca) %>%
#     filter(mme.iv > 0) %>%
#     spread(type, mme.iv) %>%
#     group_by(millennium.id) %>%
#     mutate(
#         total = sum(intermittent, continuous, pca, na.rm = TRUE),
#         data.month = data_month
#     ) %>%
#     left_join(los_month, by = "millennium.id") 

# IV intermittent only
# attending
# nurse unit
# ed patients - discharge prescription
