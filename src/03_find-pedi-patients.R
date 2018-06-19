# for vizient collaboration qi project

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/pedi"
tz <- "US/Central"
pedi <- c("HC CCN")

dirr::gzip_files(dir_raw)

# run MBO query
#   * Patients - by Location
#       - Facility (Curr): HC Childrens
#       - Admit Date: 12/1/2017 - 2/28/2018
#       - Nurse Unit All: HC CCN

pts_pedi <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

mbo_id <- concat_encounters(pts_pedi$millennium.id)

# run MBO query
#   * Encounters
#   * Demographics - Pedi
#   * Diagnosis - DRG
#   * Diagnosis - ICD-9/10-CM
#   * Location History
#   * Medications - Inpatient - All
#   * Pain PCA Pump

# location ---------------------------------------------

data_pedi <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    filter(
        unit.name %in% pedi,
        arrive.datetime < mdy("3/1/2018", tz = tz),
        depart.datetime >= mdy("12/1/2017", tz = tz)
    )

data_encounters <- read_data(dir_raw, "encounters", FALSE) %>%
    as.encounters() %>%
    semi_join(data_pedi, by = "millennium.id") %>%
    select(millennium.id, admit.datetime)

# meds -------------------------------------------------

opioids <- med_lookup(
    c(
        "narcotic analgesics", 
        "narcotic analgesic combinations"
    )
) %>%
    mutate_at("med.name", str_to_lower)

# keep tramadol?

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    semi_join(data_pedi, by = "millennium.id") %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0L) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id))) 
    # filter(med.location %in% pedi) 

data_meds_opioids <- raw_meds %>%
    semi_join(opioids, by = c("med" = "med.name")) 

mbo_order_id <- concat_encounters(data_meds_opioids$orig.order.id)

# celecoxib, ibuprofen

data_meds_modal <- raw_meds %>%
    filter(
        med %in% c(
            "acetaminophen",
            "gabapentin",
            "pregabalin",
            "tramadol",
            "ibuprofen",
            "celecoxib",
            "ketorolac"
        )
    )

data_meds_po <- raw_meds %>%
    anti_join(
        data_meds_opioids, 
        by = c("millennium.id", "orig.order.id")
    ) %>%
    filter(
        is.na(event.tag),
        str_detect(
            route, 
            "PO|PEG|GT|NG|T FEED|CHEW|NJ|DHT|OGT|JT"
        )
    )

# run MBO query
#   * Opiod Stewardship/07_orders-details_opioids

data_orders <- read_data(dir_raw, "orders", FALSE) %>%
    # as.order_detail() %>%
    rename(
        millennium.id = `Encounter Identifier`,
        order.id = `Order Id`,
        med.product = `Mnemonic (Product)`,
        route.order = `Order Route`,
        frequency = Frequency,
        prn = `PRN Indicator`
    ) %>%
    distinct(
        millennium.id,
        order.id,
        med.product,
        prn,
        .keep_all = TRUE
    ) 

# meds_orders <- data_meds_opioids %>%
#     left_join(
#         data_orders,
#         by = c("millennium.id", "orig.order.id" = "order.id")
#     ) 

# pca --------------------------------------------------

pca_actions <- c(
    "pca continuous rate dose" = "pca.rate",
    "pca clinician bolus" = "pca.bolus",
    "pca clinician bolus unit" = "pca.bolus.unit",
    "pca demand dose unit" = "pca.dose.unit",
    "pca demand dose" = "pca.dose",
    "pca doses delivered" = "pca.delivered",
    "pca drug" = "pca.drug",
    "pca loading dose" = "pca.load",
    "pca lockout interval \\(minutes\\)" = "pca.lockout",
    "pca total demands" = "pca.demands"
)

data_pca <- read_data(dir_raw, "pain-pca", FALSE) %>%
    as.pain_scores() %>%
    semi_join(data_pedi, by = "millennium.id") %>%
    filter(event.location %in% pedi) %>%
    select(millennium.id:event.result) %>%
    mutate_at("event", str_replace_all, pattern = pca_actions) %>%
    distinct() %>%
    spread(event, event.result) %>%
    mutate_at(
        c(
            "pca.demands",
            "pca.dose",
            "pca.delivered",
            "pca.load",
            "pca.lockout",
            "pca.rate"
            # "pca.bolus"
        ),
        as.numeric
    ) %>%
    group_by(millennium.id) %>%
    mutate(
        duration = difftime(
            lead(event.datetime),
            event.datetime,
            units = "hours"
        ), 
        total.rate = pca.rate * duration
    ) %>%
    group_by(millennium.id, event.datetime) %>%
    mutate(
        total.dose = sum(
            pca.delivered * pca.dose,
            pca.load,
            # pca.bolus,
            total.rate,
            na.rm = TRUE
        )
    )

# data_pca <- pain_pca %>%
#     group_by(millennium.id, pca.drug) %>%
#     summarize_at(
#         c(
#             "pca.demands",
#             "pca.delivered",
#             "total.dose",
#             "duration"
#         ),
#         sum,
#         na.rm = TRUE
#     )

# diagnosis --------------------------------------------

data_icd <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    semi_join(data_pedi, by = "millennium.id")

data_drg <- read_data(dir_raw, "drg", FALSE) %>%
    as.drg()%>%
    semi_join(data_pedi, by = "millennium.id")

# demographics -----------------------------------------

data_demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(
        extras = list("age.days" = "Age- Days (At Admit)")
    ) %>%
    semi_join(data_pedi, by = "millennium.id")

# data sets --------------------------------------------

dirr::save_rds("data/tidy/pedi", pattern = "data_")
