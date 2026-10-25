#!/usr/bin/env Rscript

# Build the official NBS fixed-asset-investment seasonally adjusted M/M
# history from annual revision tables.  Each table is a published vintage; the
# selected rows are disjoint source snapshots, not reconstructed first-release
# values.  The script is offline unless explicitly called with --download.

suppressPackageStartupMessages({ library(curl); library(digest); library(rvest) })
root <- normalizePath(getwd(), mustWork = TRUE)
download <- "--download" %in% commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
version <- "2026-08-11.1"
series_id <- "NBS/CN/M/FAI/TOTAL/MOM_PERCENT_OFFICIAL_SA"
raw_dir <- file.path(root, "inst", "extdata", "nbs-fai-sa-history")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

sources <- data.frame(
  snapshot_id = as.character(2011:2025),
  release_date = c("2012-01-17", "2013-01-18", "2014-01-20", "2015-01-20", "2016-01-19", "2017-01-20", "2018-01-18", "2019-01-21", "2020-01-17", "2021-01-18", "2022-01-17", "2023-01-17", "2024-01-17", "2025-01-17", "2026-01-19"),
  source_url = c(
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202303/t20230301_1919326.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898251.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898430.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202302/t20230203_1898674.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899017.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899395.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899814.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900202.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202302/t20230203_1900594.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900964.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901336.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901709.html",
    "https://www.stats.gov.cn/sj/xwfbh/fbhwd/202401/t20240117_1946624.html",
    "https://www.stats.gov.cn/sj/zxfb/202501/t20250117_1958332.html",
    "https://www.stats.gov.cn/sj/zxfb/202601/t20260119_1962330.html"
  ),
  selected_start = c("2011-02-01", sprintf("%d-01-01", 2012:2025)),
  selected_end = c(sprintf("%d-12-01", 2011:2024), "2025-05-01"),
  stringsAsFactors = FALSE
)

capture <- function(row) {
  path <- file.path(raw_dir, paste0(row$snapshot_id, ".html"))
  if (!file.exists(path) || (download && refresh)) {
    response <- curl_fetch_memory(row$source_url, handle = new_handle(
      followlocation = TRUE,
      useragent = "Mozilla/5.0 (compatible; readNBS official-source-archive)",
      httpheader = c("Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8")
    ))
    if (response$status_code != 200L || !length(response$content)) {
      stop("Could not download official NBS FAI source: ", row$source_url, call. = FALSE)
    }
    writeBin(response$content, path)
  }
  list(path = path, sha256 = digest(file = path, algo = "sha256", serialize = FALSE))
}

number <- function(x, url) {
  raw <- gsub("[[:space:],，]", "", trimws(as.character(x)), perl = TRUE)
  if (!grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$", raw)) {
    stop("Non-numeric official FAI SA cell `", raw, "` in ", url, call. = FALSE)
  }
  list(raw = trimws(as.character(x)), value = as.numeric(raw))
}

parse_snapshot <- function(row, source) {
  tables <- html_table(read_html(source$path), fill = TRUE, header = FALSE, convert = FALSE)
  signatures <- vapply(tables, function(x) paste(as.matrix(x), collapse = "\r"), character(1L))
  tables <- tables[!duplicated(signatures)]
  matches <- Filter(function(table) {
    text <- paste(as.matrix(table), collapse = " ")
    nrow(table) >= 12L && ncol(table) >= 4L &&
      grepl("固定资产", text, fixed = TRUE) && grepl("环比增速", text, fixed = TRUE)
  }, tables)
  if (length(matches) != 1L) stop("Expected one FAI SA table in ", row$source_url, call. = FALSE)
  table <- as.data.frame(matches[[1L]], stringsAsFactors = FALSE)
  month_scores <- vapply(table, function(column) sum(grepl("[0-9]{1,2}月份", column)), integer(1L))
  month_col <- which.max(month_scores)
  fai_scores <- vapply(table, function(column) {
    as.integer(grepl("固定资产", paste(column, collapse = ""), fixed = TRUE)) *
      sum(grepl("^[[:space:]]*[+-]?[0-9]+([.][0-9]+)?[[:space:]]*$", column))
  }, integer(1L))
  fai_col <- which.max(fai_scores)
  rows <- which(grepl("([0-9]{1,2})月份", table[[month_col]]))
  months <- as.integer(sub(".*?([0-9]{1,2})月份.*", "\\1", table[[month_col]][rows]))
  if (length(rows) < 11L || anyDuplicated(months) || !identical(months, seq(min(months), max(months)))) {
    stop("Official FAI SA table has invalid month rows in ", row$source_url, call. = FALSE)
  }
  values <- lapply(table[[fai_col]][rows], number, url = row$source_url)
  data.frame(
    manifest_version = version, dataset_id = series_id, series_id = series_id,
    observation_date = sprintf("%s-%02d-01", row$snapshot_id, months),
    value = vapply(values, `[[`, numeric(1L), "value"), raw_value = vapply(values, `[[`, character(1L), "raw"),
    unit = "Percent", seasonal_adjustment = "official_seasonally_adjusted",
    data_status = "official_revised_history_snapshot",
    source_vintage = paste0("official_nbs_revision_snapshot_", row$release_date),
    vintage_date = row$release_date, release_date = row$release_date,
    release_source_url = row$source_url, source_capture_sha256 = source$sha256,
    snapshot_id = row$snapshot_id, stringsAsFactors = FALSE
  )
}

captures <- lapply(seq_len(nrow(sources)), function(i) capture(sources[i, ]))
vintages <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) parse_snapshot(sources[i, ], captures[[i]])))
selected <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) {
  source <- sources[i, ]
  rows <- vintages[vintages$snapshot_id == source$snapshot_id & vintages$observation_date >= source$selected_start & vintages$observation_date <= source$selected_end, , drop = FALSE]
  expected <- seq(as.Date(source$selected_start), as.Date(source$selected_end), by = "month")
  if (!identical(as.Date(rows$observation_date), expected)) stop("Incomplete selected FAI slice: ", source$snapshot_id, call. = FALSE)
  rows$selection_rule <- "published_annual_revision_snapshot_non_overlapping_slice"
  rows
}))
if (anyDuplicated(selected$observation_date) || !identical(as.Date(selected$observation_date), seq(as.Date("2011-02-01"), as.Date("2025-05-01"), by = "month"))) {
  stop("Selected FAI SA history is not a unique continuous 2011-02 to 2025-05 run.", call. = FALSE)
}

index <- sources
index$manifest_version <- version; index$dataset_id <- series_id
index$source_capture_sha256 <- vapply(captures, `[[`, character(1L), "sha256")
index$expected_vintage_cells <- vapply(index$snapshot_id, function(id) sum(vintages$snapshot_id == id), integer(1L))
index$selected_cells <- vapply(index$snapshot_id, function(id) sum(selected$snapshot_id == id), integer(1L))
index$snapshot_sha256 <- digest(selected, algo = "sha256", serialize = TRUE)
index <- index[c("manifest_version", "dataset_id", "snapshot_id", "release_date", "selected_start", "selected_end", "source_url", "expected_vintage_cells", "selected_cells", "snapshot_sha256", "source_capture_sha256")]
write.csv(index, file.path(root, "inst", "extdata", "nbs-fai-sa-history-release-index.csv"), row.names = FALSE, na = "")
for (name in c("vintages", "selected")) {
  connection <- gzfile(file.path(root, "inst", "extdata", paste0("nbs-fai-sa-history-", name, ".csv.gz")), "wt")
  write.csv(get(name), connection, row.names = FALSE, na = "")
  close(connection)
}
message("Retained ", nrow(vintages), " official FAI vintage observations and selected ", nrow(selected), " continuous observations (2011-02 to 2025-05).")
