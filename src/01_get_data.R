library(tidyverse)
library(lubridate)
library(edwr)

tz <- "US/Central"
data_month <- mdy("1/1/2017", tz = tz)
month_abbrv <- format(data_month, "%Y-%m")

dir_raw <- paste0("data/raw/", month_abbrv)

dirr::gzip_files(dir_raw)

opiods <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    mutate_at("med.name", str_to_lower) %>%
    distinct(med.name)

mbo_opiods <- concat_encounters(opiods$med.name)

# run MBO query
#   * Patients - by Medication (Generic) - Administration Date
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics;HC Childrens

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(discharge.datetime >= mdy("1/1/2017", tz = tz))

mbo_id <- concat_encounters(pts$millennium.id, 950)

# run MBO query
#   * Medications - Inpatient - Prompt
#   * Pain PCA Pump

# run EDW query
#   * Identifiers - by Millennium Encounter ID

ids <- read_data(dir_raw, "identifiers") %>%
    as.id()

edw_id <- concat_encounters(ids$pie.id, 950)

# run EDW query
#   * Service History

services <- read_data(dir_raw, "services") %>%
    as.services() %>%
    arrange(pie.id, start.datetime)

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("service.dc" = "Medical Service (Curr)"))

# pain meds --------------------------------------------

opiods <- tibble(name = c("narcotic analgesics",
                          "narcotic analgesic combinations",
                          "acetaminophen"),
                 type = c(rep("class", 2), "med"),
                 group = "sched")

cont_opiods <- tibble(name = c("fentanyl",
                               "morphine",
                               "hydromorphone",
                               "mepiridine",
                               "remifentanyl",
                               "sufentanyl"),
                      type = "med",
                      group = "cont")

lookup_meds <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    mutate_at("med.name", str_to_lower)

meds_pain <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med %in% c(lookup_meds$med.name, "acetaminophen")) %>%
    left_join(bari_id[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    inner_join(data_patients[c("pie.id", "room_out", "arrive.datetime")], by = "pie.id") %>%
    mutate(timing = case_when(med.datetime < room_out ~ "or",
                              med.datetime < arrive.datetime ~ "pacu",
                              TRUE ~ "floor"),
           time_surg = difftime(med.datetime, room_out, units = "hours"))

data_pain_meds <- meds_pain %>%
    filter(time_surg < 24) %>%
    add_count(millennium.id, med, med.dose.units, route, timing) %>%
    rename(num_doses = n) %>%
    group_by(pie.id, med, med.dose.units, route, timing, num_doses) %>%
    summarize_at("med.dose", sum, na.rm = TRUE) %>%
    arrange(pie.id, med)

opiods <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    mutate_at("med.name", str_to_lower)

data_opiods <- data_pain_meds %>%
    filter(med %in% opiods$med.name)

data_meds_cont <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    calc_runtime() %>%
    summarize_data() %>%
    filter(med %in% lookup_meds$med.name,
           cum.dose > 0) %>%
    inner_join(bari_id[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    select(pie.id, everything(), -millennium.id)



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
