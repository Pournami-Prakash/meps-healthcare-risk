source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)

capture_path <- file.path("dashboard", "prospective_lift_capture_curve.csv")
if (!file.exists(capture_path)) {
  source(file.path("R", "08_prospective_models.R"))
}

capture <- read_csv(capture_path, show_col_types = FALSE)

assumptions <- tibble(
  target = c(
    "Next-year high medical expenditure",
    "Next-year high Rx out-of-pocket burden"
  ),
  outreach_cost_per_targeted_person = c(50, 25),
  value_per_captured_true_positive = c(1000, 250),
  assumption_note = c(
    "Illustrative care-management value per correctly identified future high-cost person",
    "Illustrative pharmacy navigation/adherence value per correctly identified future high Rx out-of-pocket person"
  )
)

roi <- capture %>%
  left_join(assumptions, by = "target") %>%
  mutate(
    captured_true_positive_weight = weighted_population_targeted * precision,
    estimated_outreach_cost = weighted_population_targeted * outreach_cost_per_targeted_person,
    estimated_gross_value = captured_true_positive_weight * value_per_captured_true_positive,
    estimated_net_value = estimated_gross_value - estimated_outreach_cost,
    roi_ratio = estimated_gross_value / estimated_outreach_cost
  ) %>%
  select(
    target,
    model,
    target_share,
    weighted_population_targeted,
    capture_rate,
    precision,
    outreach_cost_per_targeted_person,
    value_per_captured_true_positive,
    estimated_outreach_cost,
    estimated_gross_value,
    estimated_net_value,
    roi_ratio,
    assumption_note
  )

dir.create("reports", showWarnings = FALSE)
dir.create("dashboard", showWarnings = FALSE)

write_csv(roi, file.path("reports", "prospective_roi_summary.csv"))
write_csv(roi, file.path("dashboard", "prospective_roi_summary.csv"))

print(roi)
