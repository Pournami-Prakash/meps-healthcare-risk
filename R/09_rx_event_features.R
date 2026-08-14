source(file.path("R", "_project_setup.R"))

library(dplyr)
library(readr)

load_rx_events <- function(year) {
  if (year == 2021) {
    rds_path <- file.path("data", "processed", "h229a_rx_events_2021.rds")
    zip_path <- file.path("data", "raw", "h229adat.zip")
    dat_path <- file.path("data", "raw", "h229a.dat")
    loader_path <- file.path("data", "raw", "h229aru.txt")
    object_name <- "h229A"
    data_url <- "https://meps.ahrq.gov/mepsweb/data_files/pufs/h229a/h229adat.zip"
    loader_url <- "https://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h229a/h229aru.txt"
  } else if (year == 2022) {
    rds_path <- file.path("data", "processed", "h239a_rx_events_2022.rds")
    zip_path <- file.path("data", "raw", "h239adat.zip")
    dat_path <- file.path("data", "raw", "h239a.dat")
    loader_path <- file.path("data", "raw", "h239aru.txt")
    object_name <- "h239A"
    data_url <- "https://meps.ahrq.gov/mepsweb/data_files/pufs/h239a/h239adat.zip"
    loader_url <- "https://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h239a/h239aru.txt"
  } else {
    stop("Unsupported year: ", year)
  }

  if (!file.exists(rds_path)) {
    if (!file.exists(zip_path)) download.file(data_url, zip_path, mode = "wb")
    if (!file.exists(dat_path)) unzip(zip_path, exdir = file.path("data", "raw"))
    if (!file.exists(loader_path)) download.file(loader_url, loader_path, mode = "wb")
    meps_path <- dat_path
    source(loader_path)
    saveRDS(get(object_name), rds_path)
  }

  readRDS(rds_path)
}

weighted_quantile <- function(x, w, probs = 0.5) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  x[findInterval(probs, cum_w) + 1]
}

aggregate_rx <- function(events, year_suffix) {
  total_col <- paste0("RXXP", year_suffix, "X")
  oop_col <- paste0("RXSF", year_suffix, "X")

  events %>%
    mutate(
      rx_total_paid = pmax(.data[[total_col]], 0),
      rx_oop_paid = pmax(.data[[oop_col]], 0),
      days_supply = pmax(RXDAYSUP, 0),
      therapeutic_class = if_else(TC1 > 0, as.character(TC1), NA_character_),
      drug_key = if_else(!is.na(RXNDC) & RXNDC != "", RXNDC, RXDRGNAM)
    ) %>%
    group_by(person_id = DUPERSID) %>%
    summarise(
      rx_event_count = n(),
      rx_unique_drug_count = n_distinct(drug_key, na.rm = TRUE),
      rx_unique_therapeutic_classes = n_distinct(therapeutic_class, na.rm = TRUE),
      rx_total_days_supply = sum(days_supply, na.rm = TRUE),
      rx_total_paid_event = sum(rx_total_paid, na.rm = TRUE),
      rx_oop_paid_event = sum(rx_oop_paid, na.rm = TRUE),
      rx_oop_share_event = if_else(rx_total_paid_event > 0, rx_oop_paid_event / rx_total_paid_event, 0),
      rx_max_single_oop_event = max(rx_oop_paid, na.rm = TRUE),
      .groups = "drop"
    )
}

prospective_path <- file.path("data", "processed", "meps_prospective_2021_2022.csv")
if (!file.exists(prospective_path)) {
  source(file.path("R", "07_make_prospective_dataset.R"))
}

prospective <- read_csv(prospective_path, show_col_types = FALSE)
prospective <- prospective %>%
  mutate(person_id = as.character(person_id))

rx_2021 <- aggregate_rx(load_rx_events(2021), "21")
rx_2022 <- aggregate_rx(load_rx_events(2022), "22")

rx_2021 <- rx_2021 %>%
  rename_with(\(x) paste0(x, "_prior"), -person_id)

rx_2022 <- rx_2022 %>%
  rename_with(\(x) paste0(x, "_next"), -person_id)

enhanced <- prospective %>%
  left_join(rx_2021, by = "person_id") %>%
  left_join(rx_2022, by = "person_id") %>%
  mutate(
    across(matches("^rx_.*_event_.*_(prior|next)$|^rx_(event_count|unique_drug_count|unique_therapeutic_classes|total_days_supply|max_single_oop_event)_(prior|next)$"), \(x) coalesce(x, 0))
  )

event_rx_oop_cutoff <- weighted_quantile(enhanced$rx_oop_paid_event_next, enhanced$weight, probs = 0.90)
event_rx_complexity_cutoff <- weighted_quantile(enhanced$rx_unique_therapeutic_classes_next, enhanced$weight, probs = 0.90)

enhanced <- enhanced %>%
  mutate(
    high_rx_oop_event_next = as.integer(rx_oop_paid_event_next >= event_rx_oop_cutoff),
    high_rx_complexity_event_next = as.integer(
      rx_unique_therapeutic_classes_next >= event_rx_complexity_cutoff &
        rx_event_count_next > 0
    )
  )

write_csv(enhanced, file.path("data", "processed", "meps_prospective_rx_event_enhanced.csv"))
write_csv(
  tibble(
    metric = c("event_rx_oop_cutoff", "event_rx_complexity_cutoff"),
    value = c(event_rx_oop_cutoff, event_rx_complexity_cutoff)
  ),
  file.path("reports", "rx_event_cutoffs.csv")
)

message("Saved Rx event-enhanced prospective dataset.")
message("Rows: ", nrow(enhanced), " Columns: ", ncol(enhanced))
message("Next-year event Rx out-of-pocket cutoff: $", format(round(event_rx_oop_cutoff), big.mark = ","))
message("Next-year therapeutic-class complexity cutoff: ", round(event_rx_complexity_cutoff, 1))
