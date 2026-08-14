project_library <- file.path(
  "renv",
  "library",
  paste0(R.version$platform, "-R-", getRversion())
)

if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}
