library(tidyverse)
library(readxl)
library(mbohelpr)

pt_list <- read_excel("data/external/tens_patient-list_baseline.xlsx")

fin_mbo <- edwr::concat_encounters(pt_list$`Enc - Patient Account`)
print(fin_mbo)

# data sets -----------------------------------------
data_dir <- "data/tidy/tens"

data_pt <- get_data(data_dir, "pt_form_data")
data_meds <- get_data(data_dir, "pain_meds")
data_surg <- get_data(data_dir, "surgeries")
data_pain <- get_data(data_dir, "pain_scores")
