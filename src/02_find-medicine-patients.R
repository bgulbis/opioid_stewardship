# for vizient collaboration qi project

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/vizient"
tz <- "US/Central"

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

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_opiods <- meds %>%
    semi_join(opiods, by = c("med" = "med.name")) %>%
    filter(med.location %in% c("HH 3CP", "HH 4WCP")) %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0L) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

mbo_order_id <- concat_encounters(meds_opiods$orig.order.id)

# run MBO query
#   * Orders Meds - Details - by Order Id

orders <- read_data(dir_raw, "orders", FALSE) %>%
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
    )


duration_cullen <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    filter(
        unit.name %in% c("HH 3CP", "HH 4WCP"),
        arrive.datetime < mdy("3/1/2018", tz = tz),
        depart.datetime >= mdy("12/1/2017", tz = tz)
    )

mbo_id <- concat_encounters(duration_cullen$millennium.id)
