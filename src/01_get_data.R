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

mbo_id <- concat_encounters(pts$millennium.id, 950)

# run MBO query
#   * 02_meds-inpt_opiods
#   * 03_pca_opiods
#   * 04_demographics_opiods

# run EDW query
#   * Identifiers - by Millennium Encounter ID

ids <- read_data(dir_raw, "identifiers") %>%
    as.id()

edw_id <- concat_encounters(ids$pie.id, 950)

# service lines ----------------------------------------

# run EDW query
#   * Service History

services <- read_data(dir_raw, "services") %>%
    as.services() %>%
    arrange(pie.id, start.datetime)

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("service.dc" = "Medical Service (Curr)"))

missing_service <- anti_join(ids, services, by = "pie.id") %>%
    left_join(demog, by = "millennium.id") %>%
    select(-fin, -person.id)
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
    filter(floor_date(med.datetime, "month") == data_month)


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
