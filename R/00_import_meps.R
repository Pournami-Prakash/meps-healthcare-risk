source(file.path("R", "_project_setup.R"))

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

zip_path <- file.path(raw_dir, "h243dat.zip")
dat_path <- file.path(raw_dir, "h243.dat")
r_loader_path <- file.path(raw_dir, "h243ru.txt")
raw_rds_path <- file.path(processed_dir, "h243_raw.rds")

if (!file.exists(zip_path)) {
  url <- "https://meps.ahrq.gov/mepsweb/data_files/pufs/h243/h243dat.zip"
  download.file(url, zip_path, mode = "wb")
}

if (!file.exists(dat_path)) {
  unzip(zip_path, exdir = raw_dir)
}

if (!file.exists(r_loader_path)) {
  url <- "https://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h243/h243ru.txt"
  download.file(url, r_loader_path, mode = "wb")
}

meps_path <- dat_path
source(r_loader_path)

saveRDS(h243, raw_rds_path)

message("Saved raw MEPS file: ", raw_rds_path)
message("Rows: ", nrow(h243), " Columns: ", ncol(h243))
