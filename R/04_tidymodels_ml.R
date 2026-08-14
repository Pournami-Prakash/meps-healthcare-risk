source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)
library(vip)

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

weighted_binary_metrics <- function(truth, score, weight, threshold = 0.20) {
  pred <- as.integer(score >= threshold)
  tibble(
    threshold = threshold,
    weighted_accuracy = weighted_mean(as.integer(pred == truth), weight),
    weighted_precision = sum(weight[pred == 1 & truth == 1], na.rm = TRUE) /
      sum(weight[pred == 1], na.rm = TRUE),
    weighted_recall = sum(weight[pred == 1 & truth == 1], na.rm = TRUE) /
      sum(weight[truth == 1], na.rm = TRUE),
    weighted_specificity = sum(weight[pred == 0 & truth == 0], na.rm = TRUE) /
      sum(weight[truth == 0], na.rm = TRUE)
  )
}

auc_rank <- function(truth, score) {
  keep <- !is.na(truth) & !is.na(score)
  truth <- truth[keep]
  score <- score[keep]
  pos <- score[truth == 1]
  neg <- score[truth == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  ranks <- rank(c(pos, neg), ties.method = "average")
  (sum(ranks[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

weighted_brier <- function(truth, score, weight) {
  weighted_mean((truth - score)^2, weight)
}

capture_at_share <- function(scored_data, share) {
  cutoff <- ceiling(nrow(scored_data) * share)
  top <- scored_data %>%
    arrange(desc(risk_score)) %>%
    slice_head(n = cutoff)

  tibble(
    target_share = share,
    targeted_rows = nrow(top),
    weighted_population_targeted = sum(top$weight, na.rm = TRUE),
    high_cost_capture_rate = sum(top$weight[top$high_cost_int == 1], na.rm = TRUE) /
      sum(scored_data$weight[scored_data$high_cost_int == 1], na.rm = TRUE),
    precision = sum(top$weight[top$high_cost_int == 1], na.rm = TRUE) /
      sum(top$weight, na.rm = TRUE),
    avg_total_expenditure = weighted.mean(top$total_expenditure, top$weight, na.rm = TRUE)
  )
}

make_predictions <- function(fit, test, model_name) {
  bind_cols(
    test %>%
      select(
        person_id,
        weight,
        high_cost,
        total_expenditure,
        cost_related_barrier,
        poverty_category,
        race_ethnicity,
        insurance_status,
        age_group,
        chronic_burden
      ),
    predict(fit, test, type = "prob"),
    predict(fit, test, type = "class")
  ) %>%
    transmute(
      model = model_name,
      person_id,
      weight,
      high_cost,
      total_expenditure,
      cost_related_barrier,
      poverty_category,
      race_ethnicity,
      insurance_status,
      age_group,
      chronic_burden,
      .pred_no,
      .pred_yes,
      .pred_class
    )
}

analytic_path <- file.path("data", "processed", "meps_analytic_2022.csv")
if (!file.exists(analytic_path)) {
  source(file.path("R", "01_make_analytic_dataset.R"))
}

df <- read_csv(analytic_path, show_col_types = FALSE) %>%
  mutate(
    high_cost = factor(if_else(high_cost == 1, "yes", "no"), levels = c("no", "yes")),
    model_weight = weight / mean(weight, na.rm = TRUE),
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
  )

model_df <- df %>%
  select(
    person_id,
    weight,
    model_weight,
    high_cost,
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
    cost_related_barrier,
    age_group,
    chronic_burden,
    total_expenditure
  ) %>%
  filter(if_all(everything(), \(x) !is.na(x))) %>%
  filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
  droplevels()

set.seed(2026)
split <- initial_split(model_df, prop = 0.75, strata = high_cost)
train <- training(split)
test <- testing(split)

base_recipe <- recipe(
  high_cost ~ age + sex + race_ethnicity + education_years +
    region + poverty_category + insurance_status + general_health +
    chronic_count + office_visits + outpatient_visits + er_visits +
    inpatient_discharges + dental_visits + home_health_days +
    cost_related_barrier,
  data = train
) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

glm_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

rf_spec <- rand_forest(trees = 500, mtry = 8, min_n = 25) %>%
  set_engine("ranger", importance = "impurity", probability = TRUE) %>%
  set_mode("classification")

xgb_spec <- boost_tree(
  trees = 500,
  tree_depth = 4,
  learn_rate = 0.05,
  loss_reduction = 0.001,
  sample_size = 0.80,
  mtry = 10
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

workflow_list <- list(
  "Tidymodels logistic regression" = workflow() %>% add_recipe(base_recipe) %>% add_model(glm_spec),
  "Ranger random forest" = workflow() %>% add_recipe(base_recipe) %>% add_model(rf_spec),
  "XGBoost gradient boosting" = workflow() %>% add_recipe(base_recipe) %>% add_model(xgb_spec)
)

fits <- lapply(workflow_list, fit, data = train)

predictions <- bind_rows(Map(make_predictions, fits, MoreArgs = list(test = test), names(fits)))

metrics_unweighted <- predictions %>%
  group_by(model) %>%
  roc_auc(truth = high_cost, .pred_yes, event_level = "second") %>%
  transmute(model, roc_auc = .estimate) %>%
  left_join(
    predictions %>%
      group_by(model) %>%
      pr_auc(truth = high_cost, .pred_yes, event_level = "second") %>%
      transmute(model, pr_auc = .estimate),
    by = "model"
  )

metrics_weighted <- predictions %>%
  mutate(truth_int = as.integer(high_cost == "yes")) %>%
  group_by(model) %>%
  group_modify(
    ~ weighted_binary_metrics(.x$truth_int, .x$.pred_yes, .x$weight, threshold = 0.20)
  ) %>%
  ungroup()

metrics <- metrics_unweighted %>%
  left_join(metrics_weighted, by = "model") %>%
  left_join(
    predictions %>%
      mutate(truth_int = as.integer(high_cost == "yes")) %>%
      group_by(model) %>%
      summarise(
        brier_score = weighted_brier(truth_int, .pred_yes, weight),
        .groups = "drop"
      ),
    by = "model"
  ) %>%
  arrange(desc(roc_auc))

best_model <- metrics$model[[1]]
scored <- predictions %>%
  filter(model == best_model) %>%
  mutate(
    risk_score = .pred_yes,
    high_cost_int = as.integer(high_cost == "yes"),
    risk_decile = ntile(risk_score, 10)
  )

decile_lift <- scored %>%
  group_by(risk_decile) %>%
  summarise(
    n = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    weighted_high_cost_rate = weighted_mean(high_cost_int, weight),
    avg_predicted_risk = weighted_mean(risk_score, weight),
    avg_total_expenditure = weighted.mean(total_expenditure, weight, na.rm = TRUE),
    .groups = "drop"
  )

calibration_deciles <- scored %>%
  group_by(risk_decile) %>%
  summarise(
    n = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    avg_predicted_risk = weighted_mean(risk_score, weight),
    observed_high_cost_rate = weighted_mean(high_cost_int, weight),
    calibration_error = observed_high_cost_rate - avg_predicted_risk,
    brier_score = weighted_brier(high_cost_int, risk_score, weight),
    .groups = "drop"
  )

capture_curve <- bind_rows(
  capture_at_share(scored, 0.05),
  capture_at_share(scored, 0.10),
  capture_at_share(scored, 0.20),
  capture_at_share(scored, 0.30)
)

threshold_grid <- tibble(threshold = seq(0.05, 0.60, by = 0.05)) %>%
  rowwise() %>%
  mutate(
    metrics = list(
      weighted_binary_metrics(scored$high_cost_int, scored$risk_score, scored$weight, threshold) %>%
        select(-threshold)
    )
  ) %>%
  tidyr::unnest(metrics) %>%
  ungroup() %>%
  mutate(
    outreach_cost_per_person = 25,
    avoided_cost_value_per_true_positive = 500,
    predicted_positive_weight = purrr::map_dbl(
      threshold,
      \(t) sum(scored$weight[scored$risk_score >= t], na.rm = TRUE)
    ),
    true_positive_weight = purrr::map_dbl(
      threshold,
      \(t) sum(scored$weight[scored$risk_score >= t & scored$high_cost_int == 1], na.rm = TRUE)
    ),
    estimated_outreach_cost = predicted_positive_weight * outreach_cost_per_person,
    estimated_value = true_positive_weight * avoided_cost_value_per_true_positive,
    estimated_net_value = estimated_value - estimated_outreach_cost
  )

subgroup_audit <- bind_rows(
  scored %>% group_by(group_type = "Income", group = poverty_category),
  scored %>% group_by(group_type = "Race/ethnicity", group = race_ethnicity),
  scored %>% group_by(group_type = "Insurance", group = insurance_status),
  scored %>% group_by(group_type = "Age", group = age_group),
  scored %>% group_by(group_type = "Chronic burden", group = chronic_burden)
) %>%
  summarise(
    n = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    weighted_high_cost_rate = weighted_mean(high_cost_int, weight),
    avg_predicted_risk = weighted_mean(risk_score, weight),
    calibration_error = weighted_high_cost_rate - avg_predicted_risk,
    auc = auc_rank(high_cost_int, risk_score),
    recall_at_20pct_threshold = {
      pred <- as.integer(risk_score >= 0.20)
      sum(weight[pred == 1 & high_cost_int == 1], na.rm = TRUE) /
        sum(weight[high_cost_int == 1], na.rm = TRUE)
    },
    precision_at_20pct_threshold = {
      pred <- as.integer(risk_score >= 0.20)
      sum(weight[pred == 1 & high_cost_int == 1], na.rm = TRUE) /
        sum(weight[pred == 1], na.rm = TRUE)
    },
    .groups = "drop"
  )

action_segments <- scored %>%
  mutate(
    risk_tier = case_when(
      risk_decile >= 9 ~ "High cost risk",
      risk_decile >= 7 ~ "Moderate cost risk",
      TRUE ~ "Lower cost risk"
    ),
    access_barrier = if_else(cost_related_barrier == 1, "Cost/access barrier", "No reported barrier"),
    recommended_action = case_when(
      risk_tier == "High cost risk" & cost_related_barrier == 1 ~
        "Affordability navigation plus care management outreach",
      risk_tier == "High cost risk" ~
        "Care management outreach",
      cost_related_barrier == 1 ~
        "Benefit education and access support",
      TRUE ~
        "Monitor"
    )
  ) %>%
  group_by(risk_tier, access_barrier, recommended_action) %>%
  summarise(
    rows = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    weighted_high_cost_rate = weighted_mean(high_cost_int, weight),
    avg_predicted_risk = weighted_mean(risk_score, weight),
    avg_total_expenditure = weighted.mean(total_expenditure, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_predicted_risk))

dir.create(file.path("reports", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create("dashboard", showWarnings = FALSE)

write_csv(metrics, file.path("reports", "ml_model_metrics.csv"))
write_csv(predictions, file.path("data", "processed", "meps_ml_predictions.csv"))
write_csv(decile_lift, file.path("dashboard", "ml_risk_decile_lift.csv"))
write_csv(calibration_deciles, file.path("dashboard", "ml_calibration_deciles.csv"))
write_csv(capture_curve, file.path("dashboard", "ml_lift_capture_curve.csv"))
write_csv(threshold_grid, file.path("dashboard", "ml_threshold_strategy.csv"))
write_csv(subgroup_audit, file.path("dashboard", "ml_subgroup_audit.csv"))
write_csv(action_segments, file.path("dashboard", "recommended_action_segments.csv"))

importance_model <- if ("XGBoost gradient boosting" %in% names(fits)) {
  "XGBoost gradient boosting"
} else if ("Ranger random forest" %in% names(fits)) {
  "Ranger random forest"
} else {
  best_model
}

best_engine <- extract_fit_engine(fits[[importance_model]])

if (inherits(best_engine, "ranger") || inherits(best_engine, "xgb.Booster")) {
  vip(best_engine, num_features = 20) +
    labs(title = paste("Top Predictors:", importance_model)) +
    theme_minimal(base_size = 12)

  ggsave(file.path("reports", "figures", "ml_variable_importance.png"), width = 8, height = 6, dpi = 150)
}

ggplot(decile_lift, aes(risk_decile, weighted_high_cost_rate)) +
  geom_col(fill = "#2c7a7b") +
  geom_line(aes(y = avg_predicted_risk), color = "#c2410c", linewidth = 1) +
  geom_point(aes(y = avg_predicted_risk), color = "#c2410c", size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = paste("Weighted High-Cost Rate by Risk Decile:", best_model),
    x = "Predicted risk decile",
    y = "Weighted high-cost rate",
    caption = "Bars show observed weighted rate; line shows average predicted risk."
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "ml_risk_decile_lift.png"), width = 8, height = 5, dpi = 150)

ggplot(calibration_deciles, aes(avg_predicted_risk, observed_high_cost_rate)) +
  geom_abline(linetype = "dashed", color = "gray50") +
  geom_point(color = "#2c7a7b", size = 2.5) +
  geom_line(color = "#2c7a7b", linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = paste("Calibration by Risk Decile:", best_model),
    x = "Average predicted risk",
    y = "Observed weighted high-cost rate"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "ml_calibration_deciles.png"), width = 7, height = 5, dpi = 150)

ggplot(capture_curve, aes(target_share, high_cost_capture_rate)) +
  geom_line(color = "#2c7a7b", linewidth = 1) +
  geom_point(color = "#2c7a7b", size = 2.5) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = paste("High-Cost Capture by Outreach Size:", best_model),
    x = "Share of people targeted",
    y = "Share of high-cost people captured"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "ml_capture_curve.png"), width = 7, height = 5, dpi = 150)

print(metrics)
