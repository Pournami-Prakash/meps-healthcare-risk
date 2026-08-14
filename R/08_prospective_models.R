source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)
library(vip)

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
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
    capture_rate = sum(top$weight[top$outcome_int == 1], na.rm = TRUE) /
      sum(scored_data$weight[scored_data$outcome_int == 1], na.rm = TRUE),
    precision = sum(top$weight[top$outcome_int == 1], na.rm = TRUE) /
      sum(top$weight, na.rm = TRUE)
  )
}

fit_target <- function(df, outcome_col, target_label, output_prefix, threshold = 0.20) {
  use_rx_event_features <- grepl("Rx", target_label, ignore.case = TRUE)

  model_df <- df %>%
    mutate(
      outcome = factor(if_else(.data[[outcome_col]] == 1, "yes", "no"), levels = c("no", "yes")),
      model_weight = weight / mean(weight, na.rm = TRUE)
    ) %>%
    select(
      person_id,
      weight,
      outcome,
      age_prior,
      sex,
      race_ethnicity,
      education_years,
      region_prior,
      poverty_category_prior,
      insurance_status_prior,
      uninsured_all_year_prior,
      general_health_prior,
      chronic_count_prior,
      office_visits_prior,
      outpatient_visits_prior,
      er_visits_prior,
      inpatient_discharges_prior,
      dental_visits_prior,
      home_health_days_prior,
      total_expenditure_prior,
      self_paid_expenditure_prior,
      rx_expenditure_prior,
      rx_self_paid_expenditure_prior,
      rx_any_prior,
      any_of(c(
        "rx_event_count_prior",
        "rx_unique_drug_count_prior",
        "rx_unique_therapeutic_classes_prior",
        "rx_total_days_supply_prior",
        "rx_total_paid_event_prior",
        "rx_oop_paid_event_prior",
        "rx_oop_share_event_prior",
        "rx_max_single_oop_event_prior"
      )),
      cost_related_barrier_prior,
      age_group_prior,
      chronic_burden_prior,
      poverty_category_prior,
      insurance_status_prior,
      race_ethnicity
    ) %>%
    filter(if_all(everything(), \(x) !is.na(x))) %>%
    filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
    droplevels()

  set.seed(2026)
  split <- initial_split(model_df, prop = 0.75, strata = outcome)
  train <- training(split)
  test <- testing(split)

  predictor_terms <- c(
    "age_prior",
    "sex",
    "race_ethnicity",
    "education_years",
    "region_prior",
    "poverty_category_prior",
    "insurance_status_prior",
    "uninsured_all_year_prior",
    "general_health_prior",
    "chronic_count_prior",
    "office_visits_prior",
    "outpatient_visits_prior",
    "er_visits_prior",
    "inpatient_discharges_prior",
    "dental_visits_prior",
    "home_health_days_prior",
    "total_expenditure_prior",
    "self_paid_expenditure_prior",
    "rx_expenditure_prior",
    "rx_self_paid_expenditure_prior",
    "rx_any_prior",
    "cost_related_barrier_prior"
  )

  if (use_rx_event_features) {
    predictor_terms <- c(
      predictor_terms,
      "rx_event_count_prior",
      "rx_unique_drug_count_prior",
      "rx_unique_therapeutic_classes_prior",
      "rx_total_days_supply_prior",
      "rx_total_paid_event_prior",
      "rx_oop_paid_event_prior",
      "rx_oop_share_event_prior",
      "rx_max_single_oop_event_prior"
    )
  }

  base_recipe <- recipe(reformulate(predictor_terms, response = "outcome"), data = train) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())

  workflow_list <- list(
    "Prospective logistic regression" = workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(logistic_reg() %>% set_engine("glm") %>% set_mode("classification")),
    "Prospective random forest" = workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(
        rand_forest(trees = 500, mtry = 9, min_n = 20) %>%
          set_engine("ranger", importance = "impurity", probability = TRUE) %>%
          set_mode("classification")
      ),
    "Prospective XGBoost" = workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(
        boost_tree(
          trees = 500,
          tree_depth = 3,
          learn_rate = 0.04,
          loss_reduction = 0.001,
          sample_size = 0.80,
          mtry = 10
        ) %>%
          set_engine("xgboost") %>%
          set_mode("classification")
      )
  )

  fits <- lapply(workflow_list, fit, data = train)

  predictions <- bind_rows(Map(
    \(fit, model_name) {
      bind_cols(
        test %>%
          select(
            person_id,
            weight,
            outcome,
            poverty_category_prior,
            race_ethnicity,
            insurance_status_prior,
            age_group_prior,
            chronic_burden_prior,
            cost_related_barrier_prior
          ),
        predict(fit, test, type = "prob"),
        predict(fit, test, type = "class")
      ) %>%
        mutate(model = model_name)
    },
    fits,
    names(fits)
  ))

  metrics_unweighted <- predictions %>%
    group_by(model) %>%
    roc_auc(truth = outcome, .pred_yes, event_level = "second") %>%
    transmute(model, roc_auc = .estimate) %>%
    left_join(
      predictions %>%
        group_by(model) %>%
        pr_auc(truth = outcome, .pred_yes, event_level = "second") %>%
        transmute(model, pr_auc = .estimate),
      by = "model"
    )

  metrics <- predictions %>%
    mutate(outcome_int = as.integer(outcome == "yes")) %>%
    group_by(model) %>%
    group_modify(~ weighted_binary_metrics(.x$outcome_int, .x$.pred_yes, .x$weight, threshold = threshold)) %>%
    ungroup() %>%
    left_join(metrics_unweighted, by = "model") %>%
    left_join(
      predictions %>%
        mutate(outcome_int = as.integer(outcome == "yes")) %>%
        group_by(model) %>%
        summarise(brier_score = weighted_brier(outcome_int, .pred_yes, weight), .groups = "drop"),
      by = "model"
    ) %>%
    mutate(target = target_label) %>%
    select(target, model, roc_auc, pr_auc, everything()) %>%
    arrange(desc(roc_auc))

  best_model <- metrics$model[[1]]
  scored <- predictions %>%
    filter(model == best_model) %>%
    mutate(
      target = target_label,
      risk_score = .pred_yes,
      outcome_int = as.integer(outcome == "yes"),
      risk_decile = ntile(risk_score, 10)
    )

  calibration <- scored %>%
    group_by(target, risk_decile) %>%
    summarise(
      n = n(),
      weighted_population = sum(weight, na.rm = TRUE),
      avg_predicted_risk = weighted_mean(risk_score, weight),
      observed_rate = weighted_mean(outcome_int, weight),
      calibration_error = observed_rate - avg_predicted_risk,
      brier_score = weighted_brier(outcome_int, risk_score, weight),
      .groups = "drop"
    )

  capture_curve <- bind_rows(
    capture_at_share(scored, 0.05),
    capture_at_share(scored, 0.10),
    capture_at_share(scored, 0.20),
    capture_at_share(scored, 0.30)
  ) %>%
    mutate(target = target_label, model = best_model, .before = 1)

  subgroup_audit <- bind_rows(
    scored %>% group_by(group_type = "Income", group = poverty_category_prior),
    scored %>% group_by(group_type = "Race/ethnicity", group = race_ethnicity),
    scored %>% group_by(group_type = "Insurance", group = insurance_status_prior),
    scored %>% group_by(group_type = "Age", group = age_group_prior),
    scored %>% group_by(group_type = "Chronic burden", group = chronic_burden_prior)
  ) %>%
    summarise(
      n = n(),
      weighted_population = sum(weight, na.rm = TRUE),
      observed_rate = weighted_mean(outcome_int, weight),
      avg_predicted_risk = weighted_mean(risk_score, weight),
      calibration_error = observed_rate - avg_predicted_risk,
      auc = auc_rank(outcome_int, risk_score),
      recall_at_threshold = {
        pred <- as.integer(risk_score >= threshold)
        sum(weight[pred == 1 & outcome_int == 1], na.rm = TRUE) /
          sum(weight[outcome_int == 1], na.rm = TRUE)
      },
      precision_at_threshold = {
        pred <- as.integer(risk_score >= threshold)
        sum(weight[pred == 1 & outcome_int == 1], na.rm = TRUE) /
          sum(weight[pred == 1], na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    mutate(target = target_label, .before = 1)

  mitigation <- subgroup_audit %>%
    filter(group_type %in% c("Income", "Race/ethnicity")) %>%
    select(target, group_type, group, global_recall = recall_at_threshold) %>%
    left_join(
      bind_rows(
        scored %>% group_by(group_type = "Income", group = poverty_category_prior),
        scored %>% group_by(group_type = "Race/ethnicity", group = race_ethnicity)
      ) %>%
        group_modify(\(.x, .y) {
          positives <- .x %>% filter(outcome_int == 1)
          group_threshold <- if (nrow(positives) > 0) {
            as.numeric(quantile(positives$risk_score, probs = 0.25, na.rm = TRUE))
          } else {
            NA_real_
          }
          pred <- as.integer(.x$risk_score >= group_threshold)
          tibble(
            group_specific_threshold = group_threshold,
            mitigated_recall = sum(.x$weight[pred == 1 & .x$outcome_int == 1], na.rm = TRUE) /
              sum(.x$weight[.x$outcome_int == 1], na.rm = TRUE),
            mitigated_precision = sum(.x$weight[pred == 1 & .x$outcome_int == 1], na.rm = TRUE) /
              sum(.x$weight[pred == 1], na.rm = TRUE)
          )
        }) %>%
        ungroup(),
      by = c("group_type", "group")
    )

  action_segments <- scored %>%
    mutate(
      risk_tier = case_when(
        risk_decile >= 9 ~ "High prospective risk",
        risk_decile >= 7 ~ "Moderate prospective risk",
        TRUE ~ "Lower prospective risk"
      ),
      access_barrier = if_else(cost_related_barrier_prior == 1, "Prior cost/access barrier", "No prior barrier"),
      recommended_action = case_when(
        risk_tier == "High prospective risk" & cost_related_barrier_prior == 1 ~
          "Affordability navigation plus proactive care management",
        risk_tier == "High prospective risk" ~
          "Proactive care management outreach",
        cost_related_barrier_prior == 1 ~
          "Benefit education and access support",
        TRUE ~ "Monitor"
      )
    ) %>%
    group_by(target, risk_tier, access_barrier, recommended_action) %>%
    summarise(
      rows = n(),
      weighted_population = sum(weight, na.rm = TRUE),
      observed_rate = weighted_mean(outcome_int, weight),
      avg_predicted_risk = weighted_mean(risk_score, weight),
      .groups = "drop"
    ) %>%
    arrange(target, desc(avg_predicted_risk))

  list(
    metrics = metrics,
    predictions = predictions %>% mutate(target = target_label, .before = 1),
    scored = scored,
    calibration = calibration,
    capture_curve = capture_curve,
    subgroup_audit = subgroup_audit,
    mitigation = mitigation,
    action_segments = action_segments,
    best_model = best_model,
    fits = fits
  )
}

enhanced_dataset_path <- file.path("data", "processed", "meps_prospective_rx_event_enhanced.csv")
if (!file.exists(enhanced_dataset_path)) {
  source(file.path("R", "09_rx_event_features.R"))
}

base_dataset_path <- file.path("data", "processed", "meps_prospective_2021_2022.csv")
if (!file.exists(base_dataset_path)) {
  source(file.path("R", "07_make_prospective_dataset.R"))
}

base_df <- read_csv(base_dataset_path, show_col_types = FALSE) %>%
  mutate(
    across(
      c(
        sex,
        race_ethnicity,
        region_prior,
        poverty_category_prior,
        insurance_status_prior,
        general_health_prior,
        age_group_prior,
        chronic_burden_prior
      ),
      as.factor
    )
  )

rx_df <- read_csv(enhanced_dataset_path, show_col_types = FALSE) %>%
  mutate(
    across(
      c(
        sex,
        race_ethnicity,
        region_prior,
        poverty_category_prior,
        insurance_status_prior,
        general_health_prior,
        age_group_prior,
        chronic_burden_prior
      ),
      as.factor
    )
  )

high_cost_results <- fit_target(base_df, "high_cost_next", "Next-year high medical expenditure", "prospective_high_cost")
rx_results <- fit_target(rx_df, "high_rx_oop_event_next", "Next-year high Rx out-of-pocket burden", "prospective_rx_oop")

all_metrics <- bind_rows(high_cost_results$metrics, rx_results$metrics)
all_calibration <- bind_rows(high_cost_results$calibration, rx_results$calibration)
all_capture <- bind_rows(high_cost_results$capture_curve, rx_results$capture_curve)
all_subgroups <- bind_rows(high_cost_results$subgroup_audit, rx_results$subgroup_audit)
all_mitigation <- bind_rows(high_cost_results$mitigation, rx_results$mitigation)
all_actions <- bind_rows(high_cost_results$action_segments, rx_results$action_segments)

dir.create("dashboard", showWarnings = FALSE)
dir.create(file.path("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

write_csv(all_metrics, file.path("reports", "prospective_model_metrics.csv"))
write_csv(all_calibration, file.path("dashboard", "prospective_calibration_deciles.csv"))
write_csv(all_capture, file.path("dashboard", "prospective_lift_capture_curve.csv"))
write_csv(all_subgroups, file.path("dashboard", "prospective_subgroup_audit.csv"))
write_csv(all_mitigation, file.path("dashboard", "prospective_disparity_mitigation.csv"))
write_csv(all_actions, file.path("dashboard", "prospective_action_segments.csv"))
write_csv(
  bind_rows(high_cost_results$scored, rx_results$scored),
  file.path("data", "processed", "meps_prospective_scores.csv")
)

ggplot(all_calibration, aes(avg_predicted_risk, observed_rate, color = target)) +
  geom_abline(linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Prospective Model Calibration by Risk Decile",
    x = "Average predicted next-year risk",
    y = "Observed next-year outcome rate",
    color = "Target"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "prospective_calibration_deciles.png"), width = 8, height = 5, dpi = 150)

ggplot(all_capture, aes(target_share, capture_rate, color = target)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Prospective Capture by Outreach Size",
    x = "Share of people targeted",
    y = "Share of next-year outcome captured",
    color = "Target"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "prospective_capture_curve.png"), width = 8, height = 5, dpi = 150)

print(all_metrics)
