#!/usr/bin/env Rscript

# Build a retained, source-faithful archive of NBS's official seasonally
# adjusted month-on-month retail-sales revision tables. Each source table is
# retained as its own dated vintage; the selected file is an explicit
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
dataset_id <- "NBS/CN/M/RETAIL/TOTAL/MOM_PERCENT_OFFICIAL_SA"

# The annual releases each retain the calendar year's source-published
# revision table. The June 2025 retail release closes the interval immediately
# before the current press-series snapshot starts in June 2025.
sources <- data.frame(
  snapshot_id = c(as.character(2011:2024), "2025-06"),
  release_date = c(
    "2012-01-17", "2013-01-18", "2014-01-20", "2015-01-20",
    "2016-01-19", "2017-01-20", "2018-01-18", "2019-01-21",
    "2020-01-17", "2021-01-18", "2022-01-17", "2023-01-17",
    "2024-01-17", "2025-01-17", "2025-06-16"
  ),
  source_url = c(
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202303/t20230301_1919326.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898251.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898430.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898674.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899017.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899395.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899814.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202302/t20230203_1900202.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202302/t20230203_1900594.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900964.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202302/t20230203_1901336.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901709.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202401/t20240117_1946624.html",
    "https://www.stats.gov.cn/sj/zxfb/202501/t20250117_1958332.html",
    "https://www.stats.gov.cn/sj/zxfb/202506/t20250616_1960169.html"
  ),
  selected_start = c("2011-02-01", sprintf("%d-01-01", 2012:2024), "2025-01-01"),
  selected_end = c(sprintf("%d-12-01", 2011:2024), "2025-05-01"),
  stringsAsFactors = FALSE
)

raw_dir <- file.path(root, "inst", "extdata", "nbs-retail-sa-history")
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
      stop("Could not download official NBS source: ", row$source_url, call. = FALSE)
    }
    writeBin(response$content, path)
  }
  list(path = path, sha256 = digest(file = path, algo = "sha256", serialize = FALSE))
}

parse_number <- function(x, source_url) {
  x <- gsub("[[:space:]，,]", "", trimws(as.character(x)), perl = TRUE)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", x)) {
    stop("Non-numeric official retail SA cell `", x, "` in ", source_url, call. = FALSE)
  }
  as.numeric(x)
}

parse_annual_table <- function(row, tables) {
  candidates <- Filter(function(table) {
    text <- paste(as.matrix(table), collapse = " ")
    nrow(table) >= 13L && nrow(table) <= 14L && ncol(table) %in% c(4L, 5L) &&
      grepl("社会消费品零售总额", text, fixed = TRUE) &&
      grepl("环比增速", text, fixed = TRUE)
  }, tables)
  if (length(candidates) != 1L) {
    stop("Expected exactly one annual retail SA revision table in ", row$source_url,
      "; found ", length(candidates), call. = FALSE)
  }
  table <- as.data.frame(candidates[[1L]], stringsAsFactors = FALSE)
  columns <- lapply(table, function(x) trimws(as.character(x)))
  month_column <- which(vapply(columns, function(x) {
    sum(grepl("[0-9]{1,2}(?=月)", x, perl = TRUE)) == 12L
  }, logical(1)))
  retail_column <- which(vapply(columns, function(x) {
    any(grepl("社会消费品零售总额", x, fixed = TRUE))
  }, logical(1)))
  if (length(month_column) != 1L || length(retail_column) != 1L) {
    stop("Could not identify annual retail SA columns in ", row$source_url, call. = FALSE)
  }
  month_rows <- grepl("[0-9]{1,2}(?=月)", columns[[month_column]], perl = TRUE)
  months <- columns[[month_column]][month_rows]
  values <- columns[[retail_column]][month_rows]
  dates <- as.Date(sprintf("%04d-%02d-01", as.integer(row$snapshot_id),
    as.integer(regmatches(months, regexpr("[0-9]{1,2}(?=月)", months, perl = TRUE)))))
  list(dates = dates, values = values)
}

parse_inception_table <- function(row, tables) {
  candidates <- Filter(function(table) {
    text <- paste(as.matrix(table), collapse = " ")
    nrow(table) == 12L && ncol(table) == 4L &&
      grepl("社会消费品零售总额", text, fixed = TRUE) &&
      grepl("环比增速", text, fixed = TRUE)
  }, tables)
  if (length(candidates) != 1L) {
    stop("Expected exactly one retail SA inception table in ", row$source_url,
      "; found ", length(candidates), call. = FALSE)
  }
  table <- as.data.frame(candidates[[1L]], stringsAsFactors = FALSE)
  months <- trimws(as.character(table[[1L]][-1L]))
  values <- trimws(as.character(table[[4L]][-1L]))
  dates <- as.Date(sprintf("2011-%02d-01",
    as.integer(regmatches(months, regexpr("[0-9]{1,2}(?=月)", months, perl = TRUE)))))
  list(dates = dates, values = values)
}

parse_retail_table <- function(row, tables) {
  candidates <- Filter(function(table) {
    text <- paste(as.matrix(table), collapse = " ")
    ncol(table) == 3L && nrow(table) >= 14L &&
      grepl("环比", text, fixed = TRUE) && grepl("2025", text, fixed = TRUE)
  }, tables)
  if (length(candidates) != 1L) {
    stop("Expected exactly one June 2025 retail SA revision table in ", row$source_url,
      "; found ", length(candidates), call. = FALSE)
  }
  table <- as.data.frame(candidates[[1L]], stringsAsFactors = FALSE)
  years <- trimws(as.character(table[[1L]][-1L]))
  months <- trimws(as.character(table[[2L]][-1L]))
  values <- trimws(as.character(table[[3L]][-1L]))
  year <- NA_integer_
  dates <- as.Date(rep(NA_character_, length(months)))
  for (i in seq_along(months)) {
    matched_year <- regmatches(years[[i]], regexpr("20[0-9]{2}", years[[i]]))
    if (length(matched_year) && nzchar(matched_year)) year <- as.integer(matched_year)
    month <- regmatches(months[[i]], regexpr("[0-9]{1,2}(?=月)", months[[i]], perl = TRUE))
    if (is.na(year) || !length(month) || !nzchar(month)) {
      stop("Could not decode official retail SA revision-table row in ", row$source_url,
        call. = FALSE)
    }
    dates[[i]] <- as.Date(sprintf("%04d-%02d-01", year, as.integer(month)))
  }
  list(dates = dates, values = values)
}

parse_snapshot <- function(row, capture) {
  doc <- read_html(capture$path)
  tables <- html_table(doc, fill = TRUE, header = FALSE, convert = FALSE)
  signatures <- vapply(tables, function(x) paste(as.matrix(x), collapse = "\r"), character(1))
  tables <- tables[!duplicated(signatures)]
  parsed <- if (row$snapshot_id == "2011") {
    parse_inception_table(row, tables)
  } else if (row$snapshot_id == "2025-06") {
    parse_retail_table(row, tables)
  } else {
    parse_annual_table(row, tables)
  }
  numeric <- vapply(parsed$values, parse_number, numeric(1), source_url = row$source_url)
  if (anyDuplicated(parsed$dates) ||
      !identical(parsed$dates, seq(min(parsed$dates), max(parsed$dates), by = "month"))) {
    stop("Official retail revision-table dates are not a continuous monthly run in ",
      row$source_url, call. = FALSE)
  }
  data.frame(
    manifest_version = manifest_version,
    dataset_id = dataset_id,
    series_id = dataset_id,
    observation_date = format(parsed$dates),
    value = numeric,
    raw_value = parsed$values,
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
  stop("Selected NBS retail SA history is not a unique continuous 2011-02 to 2025-05 run.", call. = FALSE)
}

index <- sources
index$manifest_version <- manifest_version
index$dataset_id <- dataset_id
index$source_capture_sha256 <- vapply(captures, `[[`, character(1), "sha256")
index$expected_vintage_cells <- vapply(sources$snapshot_id, function(id) sum(vintages$snapshot_id == id), integer(1))
index$selected_cells <- vapply(sources$snapshot_id, function(id) sum(selected$snapshot_id == id), integer(1))
index$snapshot_sha256 <- digest(selected, algo = "sha256", serialize = TRUE)
index <- index[, c("manifest_version", "dataset_id", "snapshot_id", "release_date", "selected_start", "selected_end", "source_url", "expected_vintage_cells", "selected_cells", "snapshot_sha256", "source_capture_sha256")]

write.csv(index, file.path(root, "inst", "extdata", "nbs-retail-sa-history-release-index.csv"), row.names = FALSE, na = "")
gz <- gzfile(file.path(root, "inst", "extdata", "nbs-retail-sa-history-vintages.csv.gz"), "wt")
write.csv(vintages, gz, row.names = FALSE, na = "")
close(gz)
gz <- gzfile(file.path(root, "inst", "extdata", "nbs-retail-sa-history-selected.csv.gz"), "wt")
write.csv(selected, gz, row.names = FALSE, na = "")
close(gz)

message("Retained ", nrow(vintages), " official vintage observations and selected ",
  nrow(selected), " continuous history observations (2011-02 to 2025-05).")
