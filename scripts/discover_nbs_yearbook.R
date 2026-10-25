#!/usr/bin/env Rscript

# Explicit maintenance downloader. Normal readNBS Yearbook parsing accepts the
# frozen files produced here and does not make a network request.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: discover_nbs_yearbook.R YEARBOOK OUTPUT_DIR", call. = FALSE)
}
yearbook <- suppressWarnings(as.integer(args[[1L]]))
if (is.na(yearbook) || yearbook < 1949L || yearbook > 2100L) {
  stop("YEARBOOK must be a four-digit publication year.", call. = FALSE)
}
output_dir <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path("R", "conditions.R"), local = TRUE)
source(file.path("R", "utils.R"), local = TRUE)
source(file.path("R", "datasets.R"), local = TRUE)
source(file.path("R", "yearbook.R"), local = TRUE)
index_url <- sprintf("https://www.stats.gov.cn/sj/ndsj/%d/left.htm", yearbook)
index_path <- file.path(output_dir, "left.htm")
index_response <- httr2::request(index_url) |>
  httr2::req_retry(max_tries = 4L) |>
  httr2::req_perform()
index_raw <- httr2::resp_body_raw(index_response)
index_html <- .nbs_yearbook_decode_html(index_raw)
writeBin(index_raw, index_path)
routes <- nbs_yearbook_routes(index_html, yearbook, index_url)
for (row in seq_len(nrow(routes))) {
  response <- httr2::request(routes$source_url[[row]]) |>
    httr2::req_retry(max_tries = 4L) |>
    httr2::req_perform()
  path <- file.path(output_dir, paste0(routes$source_table[[row]], ".html"))
  writeBin(httr2::resp_body_raw(response), path)
}
utils::write.csv(routes, file.path(output_dir, "routes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
