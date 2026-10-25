#!/usr/bin/env Rscript

# Generate the exact coverage boundary for every retained ten-day tuple.  The
# boundary is the first official release in which that exact product-code and
# measure pair appears; basket members are never backcast across identities.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else getwd()
extdata <- file.path(root, "inst", "extdata")

read_gzip <- function(path) utils::read.csv(
  gzfile(path), stringsAsFactors = FALSE, check.names = FALSE,
  colClasses = "character", na.strings = character(), fileEncoding = "UTF-8"
)
dimension_key <- function(product_code, measure) paste(
  paste0("measure=", utils::URLencode(measure, reserved = TRUE)),
  paste0("product_code=", utils::URLencode(product_code, reserved = TRUE)),
  sep = "|"
)

history <- read_gzip(file.path(extdata, "nbs-tenday-history-observations.csv.gz"))
current <- read_gzip(file.path(extdata, "nbs-press-observations.csv.gz"))
dataset_id <- "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
current <- current[current$dataset_id == dataset_id, , drop = FALSE]
observations <- rbind(
  history[c("release_period", "product_code", "measure")],
  current[c("release_period", "product_code", "measure")]
)
history_index <- utils::read.csv(
  file.path(extdata, "nbs-tenday-history-release-index.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
  na.strings = character(), fileEncoding = "UTF-8"
)
current_index <- utils::read.csv(
  file.path(extdata, "nbs-release-index.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
  na.strings = character(), fileEncoding = "UTF-8"
)
current_index <- current_index[current_index$dataset_id == dataset_id, , drop = FALSE]
release_index <- rbind(
  history_index[c("period", "reference_period_start", "source_url")],
  current_index[c("period", "reference_period_start", "source_url")]
)
matched <- match(observations$release_period, release_index$period)
if (anyNA(matched)) stop("A ten-day observation has no release-index row.", call. = FALSE)
observations$reference_period_start <- release_index$reference_period_start[matched]
observations$source_url <- release_index$source_url[matched]
observations$key <- paste(observations$product_code, observations$measure, sep = "\r")
ordering <- order(observations$key, observations$reference_period_start, method = "radix")
first <- ordering[!duplicated(observations$key[ordering])]
rows <- observations[first, , drop = FALSE]
rows <- rows[order(rows$product_code, rows$measure, method = "radix"), , drop = FALSE]
output <- data.frame(
  policy_version = "2026-08-11.1", dataset_id = dataset_id,
  dimension_key = dimension_key(rows$product_code, rows$measure),
  product_code = rows$product_code, measure = rows$measure,
  required_from = rows$reference_period_start,
  first_release_period = rows$release_period,
  reason = "official_exact_source_tuple_inception",
  authority = rows$source_url,
  stringsAsFactors = FALSE
)
if (nrow(output) != 330L || anyDuplicated(output$dimension_key) ||
    any(!grepl("^https://www\\.stats\\.gov\\.cn/", output$authority))) {
  stop("Generated ten-day tuple-inception policy is invalid.", call. = FALSE)
}
utils::write.csv(
  output, file.path(extdata, "nbs-tenday-tuple-inceptions.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
cat(sprintf("Wrote %d exact ten-day tuple inception boundaries.\n", nrow(output)))
