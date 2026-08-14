source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)
library(survey)
library(broom)

analytic_path <- file.path("data", "processed", "meps_analytic_2022.csv")
if (!file.exists(analytic_path)) {
  source(file.path("R", "01_make_analytic_dataset.R"))
}

df <- read_csv(analytic_path, show_col_types = FALSE) %>%
  mutate(
    high_cost = as.integer(high_cost),
    cost_related_barrier = as.integer(cost_related_barrier),
    across(
      c(
        sex,
        race_ethnicity,
        region,
        poverty_category,
        insurance_status,
        general_health,
        age_group,
        chronic_burden
      ),
      as.factor
    )
  ) %>%
  filter(weight > 0)

survey_options <- options(survey.lonely.psu = "adjust")

meps_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = df,
  nest = TRUE
)

weighted_outcomes <- svymean(
  ~high_cost + cost_related_barrier,
  design = meps_design,
  na.rm = TRUE
)

weighted_outcomes_tbl <- tibble(
  outcome = names(coef(weighted_outcomes)),
  weighted_rate = as.numeric(coef(weighted_outcomes)),
  standard_error = as.numeric(SE(weighted_outcomes))
)

survey_formula <- high_cost ~ age + sex + race_ethnicity + education_years +
  region + poverty_category + insurance_status + general_health +
  chronic_count + office_visits + outpatient_visits + er_visits +
  inpatient_discharges + dental_visits + home_health_days +
  cost_related_barrier

model_df <- df %>%
  filter(if_all(all.vars(survey_formula), \(x) !is.na(x))) %>%
  filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
  droplevels()

model_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = model_df,
  nest = TRUE
)

survey_high_cost_model <- svyglm(
  survey_formula,
  design = model_design,
  family = quasibinomial()
)

survey_high_cost_coefficients <- tidy(survey_high_cost_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    conf_low = exp(estimate - 1.96 * std.error),
    conf_high = exp(estimate + 1.96 * std.error)
  ) %>%
  arrange(desc(abs(estimate)))

barrier_formula <- cost_related_barrier ~ age + sex + race_ethnicity + education_years +
  region + poverty_category + insurance_status + general_health +
  chronic_count + office_visits + outpatient_visits + er_visits +
  inpatient_discharges + dental_visits + home_health_days

barrier_model_df <- df %>%
  filter(if_all(all.vars(barrier_formula), \(x) !is.na(x))) %>%
  filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
  droplevels()

barrier_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = barrier_model_df,
  nest = TRUE
)

survey_barrier_model <- svyglm(
  barrier_formula,
  design = barrier_design,
  family = quasibinomial()
)

survey_barrier_coefficients <- tidy(survey_barrier_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    conf_low = exp(estimate - 1.96 * std.error),
    conf_high = exp(estimate + 1.96 * std.error)
  ) %>%
  arrange(desc(abs(estimate)))

dir.create("reports", showWarnings = FALSE)
write_csv(weighted_outcomes_tbl, file.path("reports", "survey_weighted_outcomes.csv"))
write_csv(survey_high_cost_coefficients, file.path("reports", "survey_high_cost_coefficients.csv"))
write_csv(survey_barrier_coefficients, file.path("reports", "survey_cost_barrier_coefficients.csv"))

print(weighted_outcomes_tbl)

options(survey.lonely.psu = survey_options$survey.lonely.psu)
