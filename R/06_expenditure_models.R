source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)

weighted_rmse <- function(truth, estimate, weight) {
  sqrt(sum(weight * (truth - estimate)^2, na.rm = TRUE) / sum(weight[!is.na(truth)], na.rm = TRUE))
}

weighted_mae <- function(truth, estimate, weight) {
  sum(weight * abs(truth - estimate), na.rm = TRUE) / sum(weight[!is.na(truth)], na.rm = TRUE)
}

weighted_r_squared <- function(truth, estimate, weight) {
  weighted_truth <- weighted.mean(truth, weight, na.rm = TRUE)
  sse <- sum(weight * (truth - estimate)^2, na.rm = TRUE)
  sst <- sum(weight * (truth - weighted_truth)^2, na.rm = TRUE)
  1 - sse / sst
}

analytic_path <- file.path("data", "processed", "meps_analytic_2022.csv")
if (!file.exists(analytic_path)) {
  source(file.path("R", "01_make_analytic_dataset.R"))
}

df <- read_csv(analytic_path, show_col_types = FALSE) %>%
  mutate(
    log_total_expenditure = log1p(total_expenditure),
    any_expenditure = factor(if_else(total_expenditure > 0, "yes", "no"), levels = c("no", "yes")),
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
  select(
    person_id,
    weight,
    total_expenditure,
    log_total_expenditure,
    any_expenditure,
    age,
    sex,
    race_ethnicity,
    education_years,
    region,
    poverty_category,
    insurance_status,
    general_health,
    chronic_count,
    office_visits,
    outpatient_visits,
    er_visits,
    inpatient_discharges,
    dental_visits,
    home_health_days,
    cost_related_barrier
  ) %>%
  filter(if_all(everything(), \(x) !is.na(x))) %>%
  filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
  droplevels()

set.seed(2026)
split <- initial_split(df, prop = 0.75, strata = any_expenditure)
train <- training(split)
test <- testing(split)

base_recipe <- recipe(
  log_total_expenditure ~ age + sex + race_ethnicity + education_years +
    region + poverty_category + insurance_status + general_health +
    chronic_count + office_visits + outpatient_visits + er_visits +
    inpatient_discharges + dental_visits + home_health_days +
    cost_related_barrier,
  data = train
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

lm_fit <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(linear_reg() %>% set_engine("lm")) %>%
  fit(data = train)

rf_fit <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(
    rand_forest(trees = 500, mtry = 8, min_n = 25) %>%
      set_engine("ranger", importance = "impurity") %>%
      set_mode("regression")
  ) %>%
  fit(data = train)

predictions <- bind_rows(
  bind_cols(test, predict(lm_fit, test)) %>% mutate(model = "Log-linear regression"),
  bind_cols(test, predict(rf_fit, test)) %>% mutate(model = "Ranger random forest")
) %>%
  mutate(
    predicted_expenditure = pmax(expm1(.pred), 0),
    residual = total_expenditure - predicted_expenditure
  )

metrics <- predictions %>%
  group_by(model) %>%
  summarise(
    weighted_rmse_dollars = weighted_rmse(total_expenditure, predicted_expenditure, weight),
    weighted_mae_dollars = weighted_mae(total_expenditure, predicted_expenditure, weight),
    weighted_r_squared = weighted_r_squared(log_total_expenditure, .pred, weight),
    avg_observed_expenditure = weighted.mean(total_expenditure, weight, na.rm = TRUE),
    avg_predicted_expenditure = weighted.mean(predicted_expenditure, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(weighted_rmse_dollars)

best_model <- metrics$model[[1]]

expenditure_deciles <- predictions %>%
  filter(model == best_model) %>%
  mutate(predicted_decile = ntile(predicted_expenditure, 10)) %>%
  group_by(predicted_decile) %>%
  summarise(
    n = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    avg_observed_expenditure = weighted.mean(total_expenditure, weight, na.rm = TRUE),
    avg_predicted_expenditure = weighted.mean(predicted_expenditure, weight, na.rm = TRUE),
    .groups = "drop"
  )

dir.create(file.path("reports", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create("dashboard", showWarnings = FALSE)

write_csv(metrics, file.path("reports", "expenditure_model_metrics.csv"))
write_csv(expenditure_deciles, file.path("dashboard", "expenditure_decile_calibration.csv"))
write_csv(
  predictions %>% select(model, person_id, weight, total_expenditure, predicted_expenditure, residual),
  file.path("data", "processed", "meps_expenditure_predictions.csv")
)

ggplot(expenditure_deciles, aes(predicted_decile, avg_observed_expenditure)) +
  geom_col(fill = "#31572c") +
  geom_line(aes(y = avg_predicted_expenditure), color = "#bc6c25", linewidth = 1) +
  geom_point(aes(y = avg_predicted_expenditure), color = "#bc6c25", size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = paste("Expenditure Calibration by Predicted Spending Decile:", best_model),
    x = "Predicted spending decile",
    y = "Average annual expenditure",
    caption = "Bars show observed expenditure; line shows predicted expenditure."
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "expenditure_decile_calibration.png"), width = 8, height = 5, dpi = 150)

print(metrics)
