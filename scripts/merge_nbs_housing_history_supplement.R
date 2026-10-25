#!/usr/bin/env Rscript

# Merge a separately generated, reviewed housing-only supplement into the
# retained Chinese housing archive. Generate the supplement with
# update_nbs_press_archive.R from explicit downloaded NBS pages first.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop(
    "Usage: merge_nbs_housing_history_supplement.R GENERATED_DIR",
    call. = FALSE
  )
}

generated_dir <- normalizePath(args[[1L]], mustWork = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate the package root.")
script_path <- sub("^--file=", "", script_arg[[1L]])
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
extdata <- file.path(root, "inst", "extdata")
dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
expected_periods <- sprintf("2019-%02d", 1:4)

read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
}

old_observations <- read_csv(file.path(
  extdata, "nbs-housing-history-observations.csv.gz"
))
new_observations <- read_csv(file.path(
  generated_dir, "nbs-press-observations.csv.gz"
))
old_releases <- read_csv(file.path(
  extdata, "nbs-housing-history-release-index.csv"
))
new_releases <- read_csv(file.path(generated_dir, "nbs-release-index.csv"))

if (any(new_observations$dataset_id != dataset_id) ||
    any(new_releases$dataset_id != dataset_id) ||
    !setequal(unique(new_observations$release_period), expected_periods) ||
    !setequal(new_releases$period, expected_periods) ||
    any(new_releases$expected_press_cells != "1680") ||
    any(new_releases$index_base != "2015") ||
    any(new_releases$storage != "snapshot") ||
    nrow(new_observations) != 4L * 1680L) {
  stop("The generated housing supplement does not match the reviewed contract.")
}
baseline_periods <- format(
  seq(as.Date("2011-01-01"), as.Date("2018-12-01"), by = "month"),
  "%Y-%m"
)
if (!setequal(
    unique(old_observations$release_period),
    c(baseline_periods, expected_periods)
) || !setequal(old_releases$period, c(baseline_periods, expected_periods))) {
  stop("The retained housing archive is not the reviewed 2011--2018 baseline.")
}
old_observations <- old_observations[
  !old_observations$release_period %in% expected_periods, , drop = FALSE
]
old_releases <- old_releases[
  !old_releases$period %in% expected_periods, , drop = FALSE
]
if (!identical(sort(unique(old_observations$release_period)), baseline_periods)) {
  stop("The retained housing archive is not the reviewed 2011--2018 baseline.")
}

columns <- union(names(old_observations), names(new_observations))
for (column in setdiff(columns, names(old_observations))) old_observations[[column]] <- ""
for (column in setdiff(columns, names(new_observations))) new_observations[[column]] <- ""
observations <- rbind(old_observations[columns], new_observations[columns])
observations <- observations[order(
  observations$release_period,
  as.integer(observations$table_index),
  as.integer(observations$row_index),
  as.integer(observations$column_index)
), , drop = FALSE]

archive_path <- file.path(extdata, "nbs-housing-history-observations.csv.gz")
connection <- gzfile(archive_path, open = "wt", encoding = "UTF-8")
utils::write.csv(observations, connection, row.names = FALSE, na = "")
close(connection)
archive_hash <- digest::digest(
  file = archive_path, algo = "sha256", serialize = FALSE
)

releases <- rbind(old_releases[names(new_releases)], new_releases)
releases <- releases[order(releases$period), , drop = FALSE]
releases$snapshot_sha256 <- archive_hash
utils::write.csv(
  releases,
  file.path(extdata, "nbs-housing-history-release-index.csv"),
  row.names = FALSE,
  na = ""
)

cat(sprintf(
  "Wrote %d housing observations across %d monthly releases; SHA-256 %s.\n",
  nrow(observations), nrow(releases), archive_hash
))
