source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)
library(ggplot2)
library(rpart)

auc_score <- function(truth, score) {
  keep <- !is.na(truth) & !is.na(score)
  truth <- truth[keep]
  score <- score[keep]
  pos <- score[truth == 1]
  neg <- score[truth == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  ranks <- rank(c(pos, neg), ties.method = "average")
  (sum(ranks[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) / (length(pos) * length(neg))
}

binary_metrics <- function(truth, score, threshold = 0.50) {
  pred <- as.integer(score >= threshold)
  tibble(
    threshold = threshold,
    accuracy = mean(pred == truth, na.rm = TRUE),
    precision = sum(pred == 1 & truth == 1, na.rm = TRUE) / sum(pred == 1, na.rm = TRUE),
    recall = sum(pred == 1 & truth == 1, na.rm = TRUE) / sum(truth == 1, na.rm = TRUE),
    specificity = sum(pred == 0 & truth == 0, na.rm = TRUE) / sum(truth == 0, na.rm = TRUE),
    auc = auc_score(truth, score)
  )
}

model_formula <- high_cost ~ age + sex + race_ethnicity + education_years +
  region + poverty_category + insurance_status + general_health +
  chronic_count + office_visits + outpatient_visits + er_visits +
  inpatient_discharges + dental_visits + home_health_days +
  cost_related_barrier

analytic_path <- file.path("data", "processed", "meps_analytic_2022.csv")
if (!file.exists(analytic_path)) {
  source(file.path("R", "01_make_analytic_dataset.R"))
}

df <- read_csv(analytic_path, show_col_types = FALSE) %>%
  mutate(
    high_cost = as.integer(high_cost),
    across(
      c(sex, race_ethnicity, region, poverty_category, insurance_status, general_health),
      as.factor
    ),
    model_weight = weight / mean(weight, na.rm = TRUE)
  )

model_vars <- all.vars(model_formula)
df <- df %>%
  filter(if_all(all_of(model_vars), \(x) !is.na(x))) %>%
  filter(if_all(where(is.factor), \(x) as.character(x) != "Missing")) %>%
  droplevels()

set.seed(2026)
train_idx <- sample(seq_len(nrow(df)), size = floor(0.75 * nrow(df)))
train <- df[train_idx, ]
test <- df[-train_idx, ]

glm_unweighted <- glm(model_formula, data = train, family = binomial())
glm_weighted <- glm(model_formula, data = train, family = quasibinomial(), weights = model_weight)

tree_model <- rpart(
  model_formula,
  data = train,
  method = "class",
  weights = model_weight,
  control = rpart.control(cp = 0.001, minbucket = 75)
)

test <- test %>%
  mutate(
    pred_glm_unweighted = predict(glm_unweighted, newdata = test, type = "response"),
    pred_glm_weighted = predict(glm_weighted, newdata = test, type = "response"),
    pred_tree = predict(tree_model, newdata = test, type = "prob")[, "1"]
  )

metrics <- bind_rows(
  binary_metrics(test$high_cost, test$pred_glm_unweighted, threshold = 0.20) %>%
    mutate(model = "Logistic regression"),
  binary_metrics(test$high_cost, test$pred_glm_weighted, threshold = 0.20) %>%
    mutate(model = "Weighted logistic regression"),
  binary_metrics(test$high_cost, test$pred_tree, threshold = 0.20) %>%
    mutate(model = "Weighted decision tree")
) %>%
  select(model, everything())

scored <- test %>%
  mutate(
    risk_score = pred_glm_weighted,
    risk_decile = ntile(risk_score, 10)
  )

decile_lift <- scored %>%
  group_by(risk_decile) %>%
  summarise(
    n = n(),
    observed_high_cost_rate = mean(high_cost),
    avg_predicted_risk = mean(risk_score),
    avg_total_expenditure = mean(total_expenditure),
    .groups = "drop"
  )

subgroup_metrics <- bind_rows(
  scored %>% group_by(group_type = "Income", group = poverty_category),
  scored %>% group_by(group_type = "Race/ethnicity", group = race_ethnicity),
  scored %>% group_by(group_type = "Insurance", group = insurance_status),
  scored %>% group_by(group_type = "Age", group = age_group),
  scored %>% group_by(group_type = "Chronic burden", group = chronic_burden)
) %>%
  summarise(
    n = n(),
    high_cost_rate = mean(high_cost),
    avg_predicted_risk = mean(risk_score),
    auc = auc_score(high_cost, risk_score),
    .groups = "drop"
  )

dir.create(file.path("dashboard"), showWarnings = FALSE)
dir.create(file.path("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

write_csv(metrics, file.path("reports", "model_metrics.csv"))
write_csv(scored, file.path("data", "processed", "meps_scored_test.csv"))
write_csv(decile_lift, file.path("dashboard", "risk_decile_lift.csv"))
write_csv(subgroup_metrics, file.path("dashboard", "subgroup_model_audit.csv"))

ggplot(decile_lift, aes(risk_decile, observed_high_cost_rate)) +
  geom_col(fill = "#28666e") +
  geom_line(aes(y = avg_predicted_risk), color = "#d95f02", linewidth = 1) +
  geom_point(aes(y = avg_predicted_risk), color = "#d95f02", size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "High-Cost Risk by Predicted Risk Decile",
    x = "Predicted risk decile",
    y = "High-cost rate",
    caption = "Bars show observed rate; line shows average predicted risk."
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("reports", "figures", "risk_decile_lift.png"), width = 8, height = 5, dpi = 150)

print(metrics)
