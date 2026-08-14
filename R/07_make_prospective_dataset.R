source(file.path("R", "_project_setup.R"))

library(dplyr)
library(forcats)
library(readr)

weighted_quantile <- function(x, w, probs = 0.5) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  x[findInterval(probs, cum_w) + 1]
}

yes_no <- function(x) {
  case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE ~ NA_integer_
  )
}

diagnosis_flag <- function(x) {
  case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE ~ NA_integer_
  )
}

clean_count <- function(x, cap = 10) {
  pmin(if_else(is.na(x) | x < 0, 0, as.numeric(x)), cap)
}

raw_path <- file.path("data", "processed", "h245_longitudinal_raw.rds")
out_path <- file.path("data", "processed", "meps_prospective_2021_2022.csv")

if (!file.exists(raw_path)) {
  zip_path <- file.path("data", "raw", "h245dat.zip")
  dat_path <- file.path("data", "raw", "h245.dat")
  loader_path <- file.path("data", "raw", "h245ru.txt")

  if (!file.exists(zip_path)) {
    download.file(
      "https://meps.ahrq.gov/mepsweb/data_files/pufs/h245/h245dat.zip",
      zip_path,
      mode = "wb"
    )
  }
  if (!file.exists(dat_path)) unzip(zip_path, exdir = file.path("data", "raw"))
  if (!file.exists(loader_path)) {
    download.file(
      "https://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h245/h245ru.txt",
      loader_path,
      mode = "wb"
    )
  }

  meps_path <- dat_path
  source(loader_path)
  saveRDS(h245, raw_path)
}

long <- readRDS(raw_path)

prior_cost_delay_med <- yes_no(long$DLAYCA6)
prior_cost_delay_dental <- yes_no(long$DLAYDN6)
prior_cost_delay_rx <- yes_no(long$DLAYPM6)
prior_unable_pay <- yes_no(long$PYUNBL6)

chronic_matrix <- list(
  hypertension = diagnosis_flag(long$HIBPDXY3),
  coronary_heart_disease = diagnosis_flag(long$CHDDXY3),
  heart_attack = diagnosis_flag(long$MIDXY3),
  emphysema = diagnosis_flag(long$EMPHDXY3),
  cancer = diagnosis_flag(long$CANCERY3),
  arthritis = diagnosis_flag(long$ARTHDXY3),
  asthma = diagnosis_flag(long$ASTHDXY3),
  diabetes = diagnosis_flag(long$DIABDXY3_M18)
)
prior_chronic_count <- rowSums(as.data.frame(chronic_matrix), na.rm = TRUE)

high_cost_cutoff_next <- weighted_quantile(long$TOTEXPY4, long$LONGWT, probs = 0.90)
high_rx_oop_cutoff_next <- weighted_quantile(long$RXSLFY4, long$LONGWT, probs = 0.90)

prospective <- tibble(
  person_id = long$DUPERSID,
  weight = long$LONGWT,
  strata = long$VARSTR,
  psu = long$VARPSU,
  age_prior = if_else(long$AGEY3X < 0, NA_real_, as.numeric(long$AGEY3X)),
  sex = factor(long$SEX, levels = c(1, 2), labels = c("Male", "Female")),
  race_ethnicity = factor(
    long$RACETHX,
    levels = 1:5,
    labels = c(
      "Hispanic",
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Non-Hispanic Asian",
      "Non-Hispanic Other/Multiple"
    )
  ),
  education_years = if_else(long$EDUCYR < 0, NA_real_, as.numeric(long$EDUCYR)),
  region_prior = factor(
    long$REGIONY3,
    levels = 1:4,
    labels = c("Northeast", "Midwest", "South", "West")
  ),
  poverty_category_prior = factor(
    long$POVCATY3,
    levels = 1:5,
    labels = c("Poor/Negative", "Near Poor", "Low Income", "Middle Income", "High Income")
  ),
  insurance_status_prior = factor(
    long$INSURCY3,
    levels = 1:8,
    labels = c(
      "<65 Any Private",
      "<65 Public Only",
      "<65 Uninsured",
      "65+ Medicare Only",
      "65+ Medicare and Private",
      "65+ Medicare and Other Public",
      "65+ Uninsured",
      "65+ No Medicare Any Coverage"
    )
  ),
  uninsured_all_year_prior = yes_no(long$UNINSY3),
  general_health_prior = factor(
    long$ADGENH6,
    levels = 1:5,
    labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
    ordered = TRUE
  ),
  chronic_count_prior = prior_chronic_count,
  has_hypertension_prior = chronic_matrix$hypertension,
  has_coronary_heart_disease_prior = chronic_matrix$coronary_heart_disease,
  has_heart_attack_prior = chronic_matrix$heart_attack,
  has_emphysema_prior = chronic_matrix$emphysema,
  has_cancer_prior = chronic_matrix$cancer,
  has_arthritis_prior = chronic_matrix$arthritis,
  has_asthma_prior = chronic_matrix$asthma,
  has_diabetes_prior = chronic_matrix$diabetes,
  office_visits_prior = clean_count(long$OBTOTVY3),
  outpatient_visits_prior = clean_count(long$OPTOTVY3),
  er_visits_prior = clean_count(long$ERTOTY3),
  inpatient_discharges_prior = clean_count(long$IPDISY3),
  dental_visits_prior = clean_count(long$DVTOTY3),
  home_health_days_prior = clean_count(long$HHTOTDY3, cap = 30),
  total_expenditure_prior = as.numeric(long$TOTEXPY3),
  self_paid_expenditure_prior = as.numeric(long$TOTSLFY3),
  rx_expenditure_prior = as.numeric(long$RXEXPY3),
  rx_self_paid_expenditure_prior = as.numeric(long$RXSLFY3),
  rx_any_prior = as.integer(long$RXEXPY3 > 0),
  cost_related_barrier_prior = as.integer(
    coalesce(prior_cost_delay_med, 0L) == 1L |
      coalesce(prior_cost_delay_dental, 0L) == 1L |
      coalesce(prior_cost_delay_rx, 0L) == 1L |
      coalesce(prior_unable_pay, 0L) == 1L
  ),
  total_expenditure_next = as.numeric(long$TOTEXPY4),
  rx_expenditure_next = as.numeric(long$RXEXPY4),
  rx_self_paid_expenditure_next = as.numeric(long$RXSLFY4),
  high_cost_next = as.integer(long$TOTEXPY4 >= high_cost_cutoff_next),
  high_rx_oop_next = as.integer(long$RXSLFY4 >= high_rx_oop_cutoff_next)
) %>%
  mutate(
    age_group_prior = cut(
      age_prior,
      breaks = c(-Inf, 17, 34, 49, 64, Inf),
      labels = c("0-17", "18-34", "35-49", "50-64", "65+")
    ),
    chronic_burden_prior = cut(
      chronic_count_prior,
      breaks = c(-Inf, 0, 1, 2, Inf),
      labels = c("0", "1", "2", "3+")
    ),
    out_of_pocket_burden_prior = if_else(
      total_expenditure_prior > 0,
      self_paid_expenditure_prior / total_expenditure_prior,
      0
    )
  ) %>%
  filter(weight > 0) %>%
  mutate(across(where(is.factor), \(x) fct_na_value_to_level(x, level = "Missing")))

write_csv(prospective, out_path)

message("Saved prospective dataset: ", out_path)
message("Rows: ", nrow(prospective), " Columns: ", ncol(prospective))
message("Next-year high-cost cutoff: $", format(round(high_cost_cutoff_next), big.mark = ","))
message("Next-year high Rx out-of-pocket cutoff: $", format(round(high_rx_oop_cutoff_next), big.mark = ","))
