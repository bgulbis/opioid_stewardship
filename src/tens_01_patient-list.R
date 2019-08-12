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
data_pca <- get_data(data_dir, "pca")
data_surg <- get_data(data_dir, "surgeries")
data_pain <- get_data(data_dir, "pain_scores", col_types = "?????c") %>%
    mutate_at("result", as.numeric) %>%
    filter(!is.na(result))

# tidy data -----------------------------------------

df_surg_start <- data_surg %>% 
    arrange(encounter_id, surgery_start_datetime, desc(primary_procedure)) %>%
    distinct(encounter_id, surg_case_id, .keep_all = TRUE)

df_surg_first <- df_surg_start %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    select(encounter_id, surgery_start_datetime, surgery_stop_datetime)

df_meds <- data_meds %>%
    inner_join(df_surg_first, by = "encounter_id") %>%
    arrange(encounter_id, event_datetime) %>%
    filter(event_datetime >= surgery_stop_datetime) %>%
    mutate(
        surg_med_days = difftime(
            event_datetime,
            surgery_stop_datetime,
            units = "days"
        ),
        postop_day = floor(surg_med_days)
    )

# calculate morphine equivalents
