library(tidyverse)
library(readxl)

pts_fy18 <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_patients_fy18.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(encntr_id, as.character),
        group = "fy18"
    )

pts_fy21 <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_patients_fy21.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(encntr_id, as.character),
        group = "fy21"
    )

raw_pts <- bind_rows(pts_fy18, pts_fy21)

mbo_encntr <- edwr::concat_encounters(raw_pts$encntr_id)
print(mbo_encntr)

raw_meds <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_medications.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(c(encntr_id, orig_order_id, event_id), as.character),
        across(medication, str_to_lower)
    )

raw_vitals <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_vitals.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(c(encntr_id, event_id), as.character),
        across(event, str_to_lower)
    )

raw_mds <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_attendings.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(across(encntr_id, as.character))
