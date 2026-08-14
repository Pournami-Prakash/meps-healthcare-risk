source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)

source_dir <- "dashboard"
tableau_dir <- file.path("dashboard", "tableau_sources")
dir.create(tableau_dir, recursive = TRUE, showWarnings = FALSE)

copy_dashboard_file <- function(from, to) {
  src <- file.path(source_dir, from)
  dest <- file.path(tableau_dir, to)
  if (!file.exists(src)) {
    stop("Missing source file: ", src)
  }
  file.copy(src, dest, overwrite = TRUE)
}

copy_dashboard_file("prospective_lift_capture_curve.csv", "01_lift_capture.csv")
copy_dashboard_file("prospective_calibration_deciles.csv", "02_calibration_deciles.csv")
copy_dashboard_file("prospective_subgroup_audit.csv", "03_subgroup_audit.csv")
copy_dashboard_file("prospective_disparity_mitigation.csv", "04_disparity_mitigation.csv")
copy_dashboard_file("prospective_action_segments.csv", "05_action_segments.csv")
copy_dashboard_file("prospective_roi_summary.csv", "06_roi_summary.csv")

metrics <- read_csv(file.path("reports", "prospective_model_metrics.csv"), show_col_types = FALSE) %>%
  mutate(
    model_rank = dense_rank(desc(roc_auc)),
    is_best_model = model_rank == 1
  )

write_csv(metrics, file.path(tableau_dir, "00_model_metrics.csv"))

readme <- tibble(
  file = c(
    "00_model_metrics.csv",
    "01_lift_capture.csv",
    "02_calibration_deciles.csv",
    "03_subgroup_audit.csv",
    "04_disparity_mitigation.csv",
    "05_action_segments.csv",
    "06_roi_summary.csv"
  ),
  purpose = c(
    "Model cards and KPI tiles",
    "Capture and precision by outreach size",
    "Observed vs predicted calibration by risk decile",
    "Subgroup performance audit",
    "Global vs group-specific threshold comparison",
    "Recommended outreach/action segments",
    "Illustrative ROI assumptions and outputs"
  )
)

write_csv(readme, file.path(tableau_dir, "README_tableau_sources.csv"))

message("Saved Tableau-ready CSV extracts to ", tableau_dir)
