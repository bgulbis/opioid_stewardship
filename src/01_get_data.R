library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/2017-01"

opiods <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    mutate_at("med.name", str_to_lower) %>%
    distinct(med.name)

mbo_opiods <- concat_encounters(opiods$med.name)

# run MBO query
#   * Patients - by Medication (Generic) - Administration Date
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics;HC Childrens
