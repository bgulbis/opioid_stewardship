library(tidyverse)
library(readxl)

pt_list <- read_excel("data/external/tens_patient-list_baseline.xlsx")

fin_mbo <- edwr::concat_encounters(pt_list$`Enc - Patient Account`)
