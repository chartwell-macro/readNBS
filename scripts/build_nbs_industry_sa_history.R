#!/usr/bin/env Rscript

# Build a retained, source-faithful archive of NBS's official seasonally
# adjusted month-on-month industrial-value-added revision tables.  Each source
# table is retained as its own dated vintage; the selected file is an explicit
# non-overlapping set of published revision snapshots, not reconstructed
# first-release data.

suppressPackageStartupMessages({
  library(curl)
  library(digest)
  library(rvest)
})

root <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(root, "inst", "extdata"))) {
  stop("Run this builder from the readNBS repository root.", call. = FALSE)
}

download <- "--download" %in% commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
manifest_version <- "2026-08-11.1"
dataset_id <- "NBS/CN/M/INDUSTRY/VALUE_ADDED/MOM_PERCENT_OFFICIAL_SA"

# The March releases ordinarily revise a trailing 13-month window (the 2023+
# format uses a 14-month January-to-February window).  The June 2025 release
# is retained solely to supply 2025-02 through 2025-05 before the existing
# press-series snapshot begins at 2025-06.
sources <- data.frame(
  snapshot_id = c(as.character(2012:2025), "2025-06"),
  release_date = c(
    "2012-03-09", "2013-03-09", "2014-03-13", "2015-03-11",
    "2016-03-12", "2017-03-14", "2018-03-14", "2019-03-14",
    "2020-03-16", "2021-03-15", "2022-03-15", "2023-03-15",
    "2024-03-18", "2025-03-17", "2025-06-16"
  ),
  source_url = c(
    "https://www.stats.gov.cn/sj/zxfb/202303/t20230301_1919344.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898272.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898466.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898726.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899062.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899442.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899863.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900255.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900655.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901025.html",
    "https://www.stats.gov.cn/xxgk/sjfb/zxfb2020/202203/t20220315_1828619.html",
    "https://www.stats.gov.cn/sj/zxfb/202303/t20230315_1937192.html",
    "https://www.stats.gov.cn/sj/zxfb/202403/t20240322_1948129.html",
    "https://www.stats.gov.cn/sj/zxfb/202503/t20250317_1959012.html",
    "https://www.stats.gov.cn/sj/zxfb/202506/t20250616_1960172.html"
  ),
  selected_start = c(
    "2011-02-01", sprintf("%d-02-01", 2012:2024),
    "2025-02-01"
  ),
  selected_end = c(
    "2012-01-01", sprintf("%d-01-01", 2013:2025),
    "2025-05-01"
  ),
  stringsAsFactors = FALSE
)

raw_dir <- file.path(root, "inst", "extdata", "nbs-industry-sa-history")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

source_capture <- function(row) {
  path <- file.path(raw_dir, paste0(row$snapshot_id, ".html"))
  if (!file.exists(path) || (download && refresh)) {
    response <- curl_fetch_memory(
      row$source_url,
      handle = new_handle(
        followlocation = TRUE,
        useragent = "Mozilla/5.0 (compatible; readNBS official-source-archive)",
        httpheader = c("Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8")
      )
    )
    if (response$status_code != 200L || !length(response$content)) {
      stop("Could not download official NBS source: ", row$source_url,
        call. = FALSE)
    }
    writeBin(response$content, path)
  }
  list(path = path, sha256 = digest(file = path, algo = "sha256", serialize = FALSE))
}

parse_number <- function(x, source_url) {
  x <- gsub("[[:space:]，,]", "", trimws(as.character(x)), perl = TRUE)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", x)) {
    stop("Non-numeric official industrial SA cell `", x, "` in ", source_url,
      call. = FALSE)
  }
  as.numeric(x)
}

parse_snapshot <- function(row, capture) {
  doc <- read_html(capture$path)
  tables <- html_table(doc, fill = TRUE, header = FALSE, convert = FALSE)
  signatures <- vapply(tables, function(x) paste(as.matrix(x), collapse = "\r"), character(1))
  tables <- tables[!duplicated(signatures)]
  candidates <- Filter(function(table) {
    text <- paste(as.matrix(table), collapse = " ")
    nrow(table) >= 14L && nrow(table) <= 15L && ncol(table) %in% c(2L, 3L) &&
      grepl("环比增速", text, fixed = TRUE)
  }, tables)
  if (length(candidates) != 1L) {
    stop("Expected exactly one industrial SA revision table in ", row$source_url,
      "; found ", length(candidates), call. = FALSE)
  }
  table <- as.data.frame(candidates[[1L]], stringsAsFactors = FALSE)
  if (ncol(table) == 2L) {
    labels <- trimws(as.character(table[[1L]][-1L]))
    values <- trimws(as.character(table[[2L]][-1L]))
    year <- NA_integer_
    dates <- as.Date(rep(NA_character_, length(labels)))
    for (i in seq_along(labels)) {
      y <- regmatches(labels[[i]], regexpr("20[0-9]{2}", labels[[i]]))
      if (length(y) && nzchar(y)) year <- as.integer(y)
      month <- regmatches(labels[[i]], regexpr("[0-9]{1,2}(?=月)", labels[[i]], perl = TRUE))
      if (is.na(year) || !length(month) || !nzchar(month)) {
        stop("Could not decode official revision-table label `", labels[[i]], "` in ",
          row$source_url, call. = FALSE)
      }
      dates[[i]] <- as.Date(sprintf("%04d-%02d-01", year, as.integer(month)))
    }
  } else {
    years <- trimws(as.character(table[[1L]][-1L]))
    months <- trimws(as.character(table[[2L]][-1L]))
    values <- trimws(as.character(table[[3L]][-1L]))
    year <- NA_integer_
    dates <- as.Date(rep(NA_character_, length(months)))
    for (i in seq_along(months)) {
      y <- regmatches(years[[i]], regexpr("20[0-9]{2}", years[[i]]))
      if (length(y) && nzchar(y)) year <- as.integer(y)
      month <- regmatches(months[[i]], regexpr("[0-9]{1,2}(?=月)", months[[i]], perl = TRUE))
      if (is.na(year) || !length(month) || !nzchar(month)) {
        stop("Could not decode official revision-table row in ", row$source_url,
          call. = FALSE)
      }
      dates[[i]] <- as.Date(sprintf("%04d-%02d-01", year, as.integer(month)))
    }
  }
  numeric <- vapply(values, parse_number, numeric(1), source_url = row$source_url)
  if (anyDuplicated(dates) || !identical(dates, seq(min(dates), max(dates), by = "month"))) {
    stop("Official revision-table dates are not a continuous monthly run in ",
      row$source_url, call. = FALSE)
  }
  data.frame(
    manifest_version = manifest_version,
    dataset_id = dataset_id,
    series_id = dataset_id,
    observation_date = format(dates),
    value = numeric,
    raw_value = values,
    unit = "Percent",
    seasonal_adjustment = "official_seasonally_adjusted",
    data_status = "official_revised_history_snapshot",
    source_vintage = paste0("official_nbs_revision_snapshot_", row$release_date),
    vintage_date = row$release_date,
    release_date = row$release_date,
    release_source_url = row$source_url,
    source_capture_sha256 = capture$sha256,
    snapshot_id = row$snapshot_id,
    stringsAsFactors = FALSE
  )
}

captures <- lapply(seq_len(nrow(sources)), function(i) source_capture(sources[i, ]))
vintages <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) {
  parse_snapshot(sources[i, ], captures[[i]])
}))
rownames(vintages) <- NULL

selected <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) {
  source <- sources[i, ]
  rows <- vintages[vintages$snapshot_id == source$snapshot_id &
    vintages$observation_date >= source$selected_start &
    vintages$observation_date <= source$selected_end, , drop = FALSE]
  expected <- seq(as.Date(source$selected_start), as.Date(source$selected_end), by = "month")
  if (!identical(as.Date(rows$observation_date), expected)) {
    stop("Selected revision slice is incomplete for snapshot ", source$snapshot_id,
      call. = FALSE)
  }
  rows$selection_rule <- "published_revision_snapshot_non_overlapping_slice"
  rows
}))
if (anyDuplicated(selected$observation_date) ||
    !identical(as.Date(selected$observation_date), seq(as.Date("2011-02-01"), as.Date("2025-05-01"), by = "month"))) {
  stop("Selected NBS industrial SA history is not a unique continuous 2011-02 to 2025-05 run.", call. = FALSE)
}

index <- sources
index$manifest_version <- manifest_version
index$dataset_id <- dataset_id
index$source_capture_sha256 <- vapply(captures, `[[`, character(1), "sha256")
index$expected_vintage_cells <- vapply(sources$snapshot_id, function(id) sum(vintages$snapshot_id == id), integer(1))
index$selected_cells <- vapply(sources$snapshot_id, function(id) sum(selected$snapshot_id == id), integer(1))
index$snapshot_sha256 <- digest(selected, algo = "sha256", serialize = TRUE)
index <- index[, c("manifest_version", "dataset_id", "snapshot_id", "release_date", "selected_start", "selected_end", "source_url", "expected_vintage_cells", "selected_cells", "snapshot_sha256", "source_capture_sha256")]

write.csv(index, file.path(root, "inst", "extdata", "nbs-industry-sa-history-release-index.csv"), row.names = FALSE, na = "")
gz <- gzfile(file.path(root, "inst", "extdata", "nbs-industry-sa-history-vintages.csv.gz"), "wt")
write.csv(vintages, gz, row.names = FALSE, na = "")
close(gz)
gz <- gzfile(file.path(root, "inst", "extdata", "nbs-industry-sa-history-selected.csv.gz"), "wt")
write.csv(selected, gz, row.names = FALSE, na = "")
close(gz)

message("Retained ", nrow(vintages), " official vintage observations and selected ",
  nrow(selected), " continuous history observations (2011-02 to 2025-05).")
