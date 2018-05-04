# for vizient collaboration qi project

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/vizient"
tz <- "US/Central"
cullen <- c("HH 3CP", "HH 4WCP")

dirr::gzip_files(dir_raw)

# run MBO query
#   * Patients - by Location
#       - Facility (Curr): HH HERMANN
#       - Admit Date: 12/1/2017 - 2/28/2018
#       - Nurse Unit All: HH 3CP;HH 4WCP

pts_cullen <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

mbo_id <- concat_encounters(pts_cullen$millennium.id, 650)

# run MBO query
#   * Demographics
#   * Diagnosis - DRG
#   * Diagnosis - ICD-9/10-CM
#   * Location History
#   * Medications - Inpatient - All
#   * Pain PCA Pump

opiods <- med_lookup(
    c(
        "narcotic analgesics", 
        "narcotic analgesic combinations"
    )
) %>%
    mutate_at("med.name", str_to_lower)

# keep tramadol?

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0L) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

data_meds_opiods <- meds %>%
    semi_join(opiods, by = c("med" = "med.name")) %>%
    filter(med.location %in% cullen) 

mbo_order_id <- concat_encounters(meds_opiods$orig.order.id)

data_meds_modal <- meds %>%
    filter(
        med.location %in% cullen,
        med %in% c(
            "acetaminophen",
            "gabapentin",
            "pregabalin",
            "tramadol"
        )
    )

data_meds_po <- meds %>%
    anti_join(
        data_meds_opiods, 
        by = c("millennium.id", "orig.order.id")
    ) %>%
    filter(
        is.na(event.tag),
        med.location %in% cullen,
        str_detect(
            route, 
            "PO|PEG|GT|NG|T FEED|CHEW|NJ|DHT|OGT|JT"
        )
    )

# run MBO query
#   * Orders Meds - Details - by Order Id

data_orders <- read_data(dir_raw, "orders", FALSE) %>%
    as.order_detail() %>%
    distinct(
        millennium.id,
        order.id,
        order.datetime,
        order,
        prn,
        .keep_all = TRUE
    ) %>%
    rename(route.order = route)

meds_orders <- meds_opiods %>%
    left_join(
        orders, 
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    filter(med != "tramadol")

duration_cullen <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    filter(
        unit.name %in% cullen,
        arrive.datetime < mdy("3/1/2018", tz = tz),
        depart.datetime >= mdy("12/1/2017", tz = tz)
    )

x <- duration_cullen %>%
    semi_join(meds_orders, by = "millennium.id") %>%
    distinct(millennium.id)

y <- distinct(duration_cullen, millennium.id)

mbo_id <- concat_encounters(duration_cullen$millennium.id)
