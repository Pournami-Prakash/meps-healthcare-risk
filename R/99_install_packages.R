required_packages <- c(
  "readr",
  "dplyr",
  "ggplot2",
  "forcats",
  "broom",
  "survey",
  "tidymodels",
  "ranger",
  "xgboost",
  "vip",
  "GPfit"
)

project_library <- file.path(
  "renv",
  "library",
  paste0(R.version$platform, "-R-", getRversion())
)

dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(normalizePath(project_library), .libPaths()))

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) == 0) {
  message("All required packages are installed.")
} else {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org", lib = project_library)
}
