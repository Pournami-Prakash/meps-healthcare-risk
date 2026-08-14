source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)

analytic_path <- file.path("data", "processed", "meps_analytic_2022.csv")
if (!file.exists(analytic_path)) {
  source(file.path("R", "01_make_analytic_dataset.R"))
}

df <- read_csv(analytic_path, show_col_types = FALSE)

weighted_rate <- function(flag, weight) {
  sum(flag * weight, na.rm = TRUE) / sum(weight[!is.na(flag)], na.rm = TRUE)
}

overview <- tibble(
  metric = c(
    "Population rows",
    "Weighted US population",
    "Weighted high-cost rate",
    "Weighted cost-related barrier rate",
    "Mean annual expenditure",
    "Mean annual self-paid expenditure"
  ),
  value = c(
    nrow(df),
    sum(df$weight, na.rm = TRUE),
    weighted_rate(df$high_cost, df$weight),
    weighted_rate(df$cost_related_barrier, df$weight),
    weighted.mean(df$total_expenditure, df$weight, na.rm = TRUE),
    weighted.mean(df$self_paid_expenditure, df$weight, na.rm = TRUE)
  )
)

segment_summary <- bind_rows(
  df %>% group_by(segment_type = "Income", segment = poverty_category),
  df %>% group_by(segment_type = "Insurance", segment = insurance_status),
  df %>% group_by(segment_type = "Race/ethnicity", segment = race_ethnicity),
  df %>% group_by(segment_type = "Age", segment = age_group),
  df %>% group_by(segment_type = "Chronic burden", segment = chronic_burden)
) %>%
  summarise(
    rows = n(),
    weighted_population = sum(weight, na.rm = TRUE),
    high_cost_rate = weighted_rate(high_cost, weight),
    cost_barrier_rate = weighted_rate(cost_related_barrier, weight),
    avg_total_expenditure = weighted.mean(total_expenditure, weight, na.rm = TRUE),
    avg_self_paid_expenditure = weighted.mean(self_paid_expenditure, weight, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(overview, file.path("dashboard", "population_overview.csv"))
write_csv(segment_summary, file.path("dashboard", "segment_summary.csv"))

message("Saved dashboard exports to dashboard/.")
