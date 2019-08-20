library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)

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

df_meds_mme <- calc_morph_eq(df_meds) %>%
    filter(!is.na(mme_iv))

df_meds_other <- calc_morph_eq(df_meds) %>%
    filter(is.na(mme_iv))

df_meds_pain <- data_meds %>%
    filter(!str_detect(medication, "Sodium Chloride")) %>%
    select(
        encounter_id, 
        med_datetime = event_datetime, 
        medication, 
        orig_order_id
    )

df_pca <- data_pca %>%
    select(-event_id) %>%
    arrange(encounter_id, event_datetime) %>%
    spread(event, result)

df_pain_scores <- data_pain %>%
    inner_join(df_surg_first, by = "encounter_id") %>%
    arrange(encounter_id, event_datetime) %>%
    filter(event_datetime >= surgery_stop_datetime) %>%
    left_join(
        df_meds_pain, 
        by = c("encounter_id", "order_id" = "orig_order_id")
    ) %>%
    filter(
        is.na(med_datetime) | (
            event_datetime >= med_datetime - hours(2) &
                event_datetime <= med_datetime + hours(2)
        )
    ) %>%
    arrange(encounter_id, event_datetime, med_datetime) %>%
    distinct(event_id, .keep_all = TRUE)
    
# find mobility / gait data

df1 <- distinct(data_pt, event_text1)
df2 <- distinct(data_pt, event_text2)
df3 <- distinct(data_pt, event_text3)
df4 <- distinct(data_pt, event_text4)

pt_fields <- regex("bed|gait|stairs|transfer", ignore_case = TRUE)

df_pt_transfer <- data_pt %>%
    filter(
        str_detect(event_text1, pt_fields) |
            str_detect(event_text2, pt_fields) | 
            str_detect(event_text3, pt_fields)
    ) %>%
    select(fin:result_value3, -event_detail1, -result_value1)

pt_activity <- regex("functional activities", ignore_case = TRUE)

df_pt_activity <- data_pt %>%
    filter(
        str_detect(event_text1, pt_activity) |
            str_detect(event_text2, pt_activity) 
    ) %>%
    select(fin:result_value3, -event_detail1, -result_value1) %>%
    filter(
        result_value2 != "Done" | is.na(result_value2)
    )

pt_pain <- regex("BPS|BPAS|pain", ignore_case = TRUE)

df_pt_pain <- data_pt %>%
    filter(
        str_detect(event_text1, pt_pain) |
            str_detect(event_text2, pt_pain) | 
            str_detect(event_text3, pt_pain)
    ) %>%
    select(fin:result_value2, -event_detail1, -result_value1)

# export data ------------------------------------------

export <- list(
    "PT Gait and Transfer" = df_pt_transfer,
    "PT Func Activity" = df_pt_activity,
    "PT Pain" = df_pt_pain,
    "Opioids" = df_meds_mme,
    "Multi-Modal Meds" = df_meds_other,
    "PCA" = df_pca,
    "Pain Scores" = df_pain_scores
)

write.xlsx(export, "data/external/tens_baseline_data.xlsx")
