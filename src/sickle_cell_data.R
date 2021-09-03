library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

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

raw_pca <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_pca.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(across(c(encntr_id, event_id), as.character))

raw_vitals <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_vitals.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(c(encntr_id, event_id), as.character),
        across(event, str_to_lower),
        across(result_val, as.numeric)
    ) |> 
    filter(!is.na(result_val))

raw_docs <- read_excel("U:/Data/opioid_stewardship/sickle_cell/raw/sickle-cell_attendings.xlsx") |> 
    rename_all(str_to_lower) |> 
    mutate(across(encntr_id, as.character))

md_admit <- raw_pts |> 
    select(encntr_id, admit_datetime) |> 
    left_join(raw_docs, by = "encntr_id") |> 
    filter(is.na(provider_position) | !str_detect(provider_position, "ED")) |> 
    arrange(encntr_id, begin_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, admit_attending = attending)

md_disch <- raw_docs |> 
    arrange(encntr_id, desc(begin_datetime)) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, disch_attending = attending)


# medications -------------------------------------------------------------

df_opioids <- raw_meds |> 
    filter(
        medication %in% c("morphine sulfate", "hydromorphone"),
        str_detect(admin_route, "IV")
    )

df_bolus <- df_opioids |> 
    filter(is.na(iv_event)) |> 
    mutate(
        mme_bolus = case_when(
            medication == "hydromorphone" ~ admin_dosage * 6.7,
            TRUE ~ admin_dosage
        )
    ) |> 
    group_by(encntr_id) |> 
    summarize(across(mme_bolus, sum, na.rm = TRUE))

df_cont <- df_opioids |> 
    filter(!is.na(iv_event)) |> 
    rename(
        med_datetime = dose_datetime,
        rate = infusion_rate,
        rate_unit = infusion_unit
    ) |> 
    drip_runtime(vars(encntr_id), drip_off = 30) |> 
    summarize_drips(vars(encntr_id)) |> 
    group_by(encntr_id, medication) |> 
    summarize(across(cum_dose, sum, na.rm = TRUE)) |> 
    mutate(
        mme_cont = case_when(
            medication == "hydromorphone" ~ cum_dose * 6.7,
            TRUE ~ cum_dose
        )
    ) |> 
    group_by(encntr_id) |> 
    summarize(across(mme_cont, sum, na.rm = TRUE))

df_pca <- raw_pca |> 
    select(-event_id) |> 
    distinct(encntr_id, event_datetime, event, .keep_all = TRUE) |> 
    pivot_wider(names_from = event, values_from = result_val) |> 
    filter(str_detect(`PCA Drug`, regex("hydromorphone|morphine", ignore_case = TRUE)))

df_pca_demand <- df_pca |> 
    mutate(
        across('PCA Drug', str_to_lower),
        across(c(`PCA Demand Dose`, `PCA Doses Delivered`), as.numeric),
        demand_given = `PCA Demand Dose` * `PCA Doses Delivered`,
        mme_pca_demand = case_when(
            str_detect(`PCA Drug`, "hydromorphone") ~ demand_given * 6.7,
            TRUE ~ demand_given
        )
    ) |> 
    group_by(encntr_id) |> 
    summarize(across(mme_pca_demand, sum, na.rm = TRUE))

df_pca_load <- df_pca |> 
    filter(!is.na(`PCA Loading Dose`)) |> 
    distinct(encntr_id, `PCA Drug`, .keep_all = TRUE) |> 
    mutate(
        across('PCA Drug', str_to_lower),
        across(`PCA Loading Dose`, as.numeric),
        mme_pca_load = case_when(
            str_detect(`PCA Drug`, "hydromorphone") ~ `PCA Loading Dose` * 6.7,
            TRUE ~ `PCA Loading Dose`
        )
    ) |> 
    group_by(encntr_id) |> 
    summarize(across(mme_pca_load, sum, na.rm = TRUE))

df_pca_continuous <- df_pca |> 
    filter(!is.na(`PCA Continuous Rate Dose`)) |> 
    mutate(
        across('PCA Drug', str_to_lower),
        across(`PCA Continuous Rate Dose`, as.numeric)
    ) |> 
    group_by(encntr_id, `PCA Drug`) |> 
    mutate(
        time_diff = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(time_diff, as.numeric),
        cont_given = `PCA Continuous Rate Dose` * time_diff,
        mme_pca_cont = case_when(
            str_detect(`PCA Drug`, "hydromorphone") ~ cont_given * 6.7,
            TRUE ~ cont_given
        )
    ) |> 
    group_by(encntr_id) |> 
    summarize(across(mme_pca_cont, sum, na.rm = TRUE))

df_mme <- raw_pts |> 
    select(encntr_id) |> 
    left_join(df_bolus, by = "encntr_id") |> 
    left_join(df_cont, by = "encntr_id") |> 
    left_join(df_pca_demand, by = "encntr_id") |> 
    left_join(df_pca_load, by = "encntr_id") |> 
    left_join(df_pca_continuous, by = "encntr_id") |> 
    group_by(encntr_id) |> 
    mutate(mme_total = sum(mme_bolus, mme_cont, mme_pca_demand, mme_pca_load, mme_pca_cont, na.rm = TRUE)) |> 
    select(encntr_id, mme_total)

data_pca <- df_pca |> 
    left_join(raw_pts[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, event_datetime:`PCA Continuous Rate Dose`)    

df_dose_stop <- df_opioids |> 
    arrange(encntr_id, orig_order_id, desc(dose_datetime)) |> 
    distinct(encntr_id, orig_order_id, .keep_all = TRUE) |> 
    select(encntr_id, orig_order_id, stop_datetime = dose_datetime)

data_doses_opioids <- df_opioids |> 
    distinct(encntr_id, orig_order_id, .keep_all = TRUE) |> 
    left_join(df_dose_stop, by = c("encntr_id", "orig_order_id")) |> 
    left_join(raw_pts[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(
        fin,
        orig_order_id,
        medication,
        admin_dosage:freq,
        prn_dose,
        nurse_unit,
        start_datetime = dose_datetime,
        stop_datetime
    )
    
adjunct_meds <- c("acetaminophen", "gabapentin", "ibuprofen", "tramadol", "ketorolac", "naproxen")

df_adjunct <- raw_meds |> 
    filter(medication %in% adjunct_meds)

df_adjunct_stop <- df_adjunct |> 
    arrange(encntr_id, orig_order_id, desc(dose_datetime)) |> 
    distinct(encntr_id, orig_order_id, .keep_all = TRUE) |> 
    select(encntr_id, orig_order_id, stop_datetime = dose_datetime)

data_doses_adjunct <- df_adjunct |> 
    distinct(encntr_id, orig_order_id, .keep_all = TRUE) |> 
    left_join(df_adjunct_stop, by = c("encntr_id", "orig_order_id")) |> 
    left_join(raw_pts[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(
        fin,
        orig_order_id,
        medication,
        admin_dosage,
        dosage_unit,
        admin_route,
        freq,
        prn_dose,
        nurse_unit,
        start_datetime = dose_datetime,
        stop_datetime
    )

data_naloxone <- raw_meds |> 
    filter(medication == "naloxone") |> 
    left_join(raw_pts[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(
        fin,
        orig_order_id,
        medication,
        dose_datetime,
        admin_dosage,
        dosage_unit,
        admin_route,
        freq,
        prn_dose,
        nurse_unit
    )

# vitals ------------------------------------------------------------------

df_rr <- raw_vitals |> 
    filter(
        event == "respiratory rate",
        (result_val < 14 | result_val > 26)
    ) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(resp_rate = TRUE) |> 
    select(
        encntr_id,
        resp_rate,
        resp_rate_datetime = event_datetime
    )

df_hr <- raw_vitals |> 
    filter(
        event %in% c("peripheral pulse rate", "apical heart rate"),
        (result_val < 55 | result_val > 100)
    ) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(heart_rate = TRUE) |> 
    select(
        encntr_id,
        heart_rate,
        heart_rate_datetime = event_datetime
    )

df_spo2 <- raw_vitals |> 
    filter(
        event == "spo2 percent",
        result_val < 94
    ) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(spo2 = TRUE) |> 
    select(
        encntr_id,
        spo2,
        spo2_datetime = event_datetime
    )

df_sbp <- raw_vitals |> 
    filter(
        event %in% c("systolic blood pressure", "arterial systolic bp 1"),
        (result_val < 90 | result_val > 160)
    ) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(systolic_bp = TRUE) |> 
    select(
        encntr_id,
        systolic_bp,
        systolic_bp_datetime = event_datetime
    )

df_dbp <- raw_vitals |> 
    filter(
        event %in% c("diastolic blood pressure", "arterial diastolic bp 1"),
        (result_val < 90 | result_val > 160)
    ) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(diastolic_bp = TRUE) |> 
    select(
        encntr_id,
        diastolic_bp,
        diastolic_bp_datetime = event_datetime
    )

pain_scores <- c("pain intensity nrs (0-10)", "bpas pain score", "bps pain score", "pain score total -mar")

df_pain_scores <- raw_vitals |> 
    filter(event %in% pain_scores)    

cullen <- c("HH 2CC", "HH 3CIM", "HH 3CP", "HH 4WCP", "HH 5WCP", "HH ACE", "HH MIMU")

df_pain_admit <- df_pain_scores |> 
    inner_join(raw_pts[c("encntr_id", "admit_nurse_unit")], by = c("encntr_id", "nurse_unit" = "admit_nurse_unit")) |> 
    arrange(encntr_id, event_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(
        encntr_id,
        pain_score_admit_cullen = result_val,
        pain_score_datetime = event_datetime
    )

df_pain_high_low <- df_pain_scores |> 
    inner_join(raw_pts[c("encntr_id", "cullen_datetime")], by = "encntr_id") |> 
    mutate(across(result_val, as.integer)) |> 
    filter(
        event_datetime >= cullen_datetime,
        event_datetime < cullen_datetime + hours(72),
        result_val <= 10
    )
    
df_pain_low <- df_pain_high_low |> 
    arrange(encntr_id, result_val, event_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, pain_score_low = result_val, low_datetime = event_datetime)

df_pain_high <- df_pain_high_low |> 
    arrange(encntr_id, desc(result_val), event_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, pain_score_high = result_val, high_datetime = event_datetime)

# final data --------------------------------------------------------------

data_patients <- raw_pts |> 
    left_join(md_admit, by = "encntr_id") |> 
    left_join(md_disch, by = "encntr_id") |> 
    left_join(df_mme, by = "encntr_id") |> 
    left_join(df_rr, by = "encntr_id") |> 
    left_join(df_hr, by = "encntr_id") |> 
    left_join(df_spo2, by = "encntr_id") |> 
    left_join(df_sbp, by = "encntr_id") |> 
    left_join(df_dbp, by = "encntr_id") |> 
    left_join(df_pain_admit, by = "encntr_id") |> 
    left_join(df_pain_low, by = "encntr_id") |> 
    left_join(df_pain_high, by = "encntr_id") |> 
    mutate(across(c(resp_rate, heart_rate, spo2, systolic_bp, diastolic_bp), ~coalesce(., FALSE))) |> 
    select(-encntr_id)

l <- list(
    "patients" = data_patients,
    "opioid_doses" = data_doses_opioids,
    "pca_doses" = data_pca,
    "adjunct_doses" = data_doses_adjunct,
    "naloxone_doses" = data_naloxone
)

write.xlsx(l, "U:/Data/opioid_stewardship/sickle_cell/final/data_sickle_cell.xlsx", overwrite = TRUE)
