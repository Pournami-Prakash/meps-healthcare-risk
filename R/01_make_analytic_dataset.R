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

cap_count <- function(x, cap = 10) {
  pmin(if_else(is.na(x) | x < 0, 0, as.numeric(x)), cap)
}

raw_path <- file.path("data", "processed", "h243_raw.rds")
out_path <- file.path("data", "processed", "meps_analytic_2022.csv")

if (!file.exists(raw_path)) {
  source(file.path("R", "00_import_meps.R"))
}

meps <- readRDS(raw_path)

cost_delay_med <- yes_no(meps$DLAYCA42)
cost_delay_dental <- yes_no(meps$DLAYDN42)
cost_delay_rx <- yes_no(meps$DLAYPM42)
unable_pay_bills <- yes_no(meps$PYUNBL42)

chronic_vars <- c("HIBPDX", "CHDDX", "MIDX", "EMPHDX", "CANCERDX", "ARTHDX", "ASTHDX")
chronic_matrix <- lapply(meps[chronic_vars], diagnosis_flag)
chronic_count <- rowSums(as.data.frame(chronic_matrix), na.rm = TRUE)

high_cost_cutoff <- weighted_quantile(meps$TOTEXP22, meps$PERWT22F, probs = 0.90)

analytic <- tibble(
  person_id = meps$DUPERSID,
  panel = meps$PANEL,
  weight = meps$PERWT22F,
  strata = meps$VARSTR,
  psu = meps$VARPSU,
  age = if_else(meps$AGE22X < 0, NA_real_, as.numeric(meps$AGE22X)),
  sex = factor(meps$SEX, levels = c(1, 2), labels = c("Male", "Female")),
  race_ethnicity = factor(
    meps$RACETHX,
    levels = 1:5,
    labels = c(
      "Hispanic",
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Non-Hispanic Asian",
      "Non-Hispanic Other/Multiple"
    )
  ),
  education_years = if_else(meps$EDUCYR < 0, NA_real_, as.numeric(meps$EDUCYR)),
  region = factor(
    meps$REGION22,
    levels = 1:4,
    labels = c("Northeast", "Midwest", "South", "West")
  ),
  poverty_category = factor(
    meps$POVCAT22,
    levels = 1:5,
    labels = c("Poor/Negative", "Near Poor", "Low Income", "Middle Income", "High Income")
  ),
  insurance_status = factor(
    meps$INSURC22,
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
  uninsured_all_year = yes_no(meps$UNINS22),
  general_health = factor(
    meps$ADGENH42,
    levels = 1:5,
    labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
    ordered = TRUE
  ),
  chronic_count = chronic_count,
  has_hypertension = diagnosis_flag(meps$HIBPDX),
  has_coronary_heart_disease = diagnosis_flag(meps$CHDDX),
  has_heart_attack = diagnosis_flag(meps$MIDX),
  has_emphysema = diagnosis_flag(meps$EMPHDX),
  has_cancer = diagnosis_flag(meps$CANCERDX),
  has_arthritis = diagnosis_flag(meps$ARTHDX),
  has_asthma = diagnosis_flag(meps$ASTHDX),
  office_visits = cap_count(meps$OBTOTV22),
  outpatient_visits = cap_count(meps$OPTOTV22),
  er_visits = cap_count(meps$ERTOT22),
  inpatient_discharges = cap_count(meps$IPDIS22),
  dental_visits = cap_count(meps$DVTOT22),
  home_health_days = cap_count(meps$HHTOTD22, cap = 30),
  total_expenditure = as.numeric(meps$TOTEXP22),
  self_paid_expenditure = as.numeric(meps$TOTSLF22),
  rx_expenditure = as.numeric(meps$RXEXP22),
  rx_self_paid_expenditure = as.numeric(meps$RXSLF22),
  high_cost = as.integer(meps$TOTEXP22 >= high_cost_cutoff),
  cost_delay_med = cost_delay_med,
  cost_delay_dental = cost_delay_dental,
  cost_delay_rx = cost_delay_rx,
  unable_pay_bills = unable_pay_bills,
  cost_related_barrier = as.integer(
    coalesce(cost_delay_med, 0L) == 1L |
      coalesce(cost_delay_dental, 0L) == 1L |
      coalesce(cost_delay_rx, 0L) == 1L |
      coalesce(unable_pay_bills, 0L) == 1L
  )
) %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 17, 34, 49, 64, Inf),
      labels = c("0-17", "18-34", "35-49", "50-64", "65+")
    ),
    chronic_burden = cut(
      chronic_count,
      breaks = c(-Inf, 0, 1, 2, Inf),
      labels = c("0", "1", "2", "3+")
    ),
    rx_any = as.integer(rx_expenditure > 0),
    out_of_pocket_burden = if_else(total_expenditure > 0, self_paid_expenditure / total_expenditure, 0)
  ) %>%
  mutate(across(where(is.factor), \(x) fct_na_value_to_level(x, level = "Missing")))

write_csv(analytic, out_path)

message("Saved analytic dataset: ", out_path)
message("Rows: ", nrow(analytic), " Columns: ", ncol(analytic))
message("Weighted high-cost cutoff: $", format(round(high_cost_cutoff), big.mark = ","))
