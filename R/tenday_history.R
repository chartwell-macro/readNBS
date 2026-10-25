# Retained NBS ten-day production-market-price history.  The 2014--19 archive
# preserves each official release tuple and its actual release calendar; it is
# not a backfill of the later, separately maintained current basket.

.nbs_tenday_history_cache <- new.env(parent = emptyenv())

.nbs_tenday_history_expected_periods <- function() {
  all_periods <- unlist(lapply(2014:2019, function(year) unlist(lapply(1:12, function(month) {
    sprintf("%04d-%02d-D%d", year, month, 1:3)
  }))), use.names = FALSE)
  setdiff(all_periods, c(
    "2015-02-D2", "2016-02-D1", "2017-01-D3", "2018-02-D2",
    "2019-02-D1", "2019-05-D2"
  ))
}

.nbs_load_tenday_history_release_index <- function() {
  if (exists("release_index", envir = .nbs_tenday_history_cache, inherits = FALSE)) {
    return(get("release_index", envir = .nbs_tenday_history_cache, inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-tenday-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "dataset_id", "period", "reference_period_start",
    "reference_period_end", "release_date", "source_url", "basket_version",
    "expected_press_cells", "schema_version", "storage", "snapshot_sha256",
    "source_capture_sha256"
  )
  expected_periods <- .nbs_tenday_history_expected_periods()
  if (!identical(names(index), required) || !identical(index$period, expected_periods) ||
      nrow(index) != 210L ||
      any(index$dataset_id != "NBS/CN/10D/PRODUCTION_MARKET_PRICES") ||
      any(index$storage != "snapshot") || any(index$expected_press_cells != "153") ||
      any(!grepl("^[0-9a-f]{64}$", index$snapshot_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256))) {
    .nbs_abort("nbs_manifest_error", "The ten-day history release index is invalid.")
  }
  index$expected_press_cells <- as.integer(index$expected_press_cells)
  assign("release_index", index, envir = .nbs_tenday_history_cache)
  index
}

.nbs_load_tenday_history_observations <- function() {
  if (exists("observations", envir = .nbs_tenday_history_cache, inherits = FALSE)) {
    return(get("observations", envir = .nbs_tenday_history_cache, inherits = FALSE))
  }
  archive_path <- .nbs_extdata_path("nbs-tenday-history-observations.csv.gz")
  rows <- utils::read.csv(
    archive_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  index <- .nbs_load_tenday_history_release_index()
  required <- c(
    "dataset_id", "release_period", "product_code", "product_identity", "measure",
    "raw_value", "value", "observation_date", "observation_period_start",
    "observation_period_end", "schema_version", "release_source_url"
  )
  counts <- table(factor(rows$release_period, levels = index$period))
  actual_hash <- digest::digest(file = archive_path, algo = "sha256", serialize = FALSE)
  expected_hash <- unique(index$snapshot_sha256)
  product <- rows$product_code != "summary"
  tuple_measure <- paste(rows$release_period[product], rows$product_identity[product], rows$measure[product], sep = "\r")
  summary_measure <- paste(rows$release_period[!product], rows$measure[!product], sep = "\r")
  if (!all(required %in% names(rows)) || nrow(rows) != 32130L ||
      length(expected_hash) != 1L || !identical(actual_hash, expected_hash) ||
      any(rows$dataset_id != index$dataset_id[[1L]]) ||
      !identical(unique(rows$release_period), index$period) ||
      any(!is.finite(suppressWarnings(as.numeric(rows$value)))) ||
      !identical(as.integer(counts), index$expected_press_cells) ||
      anyDuplicated(tuple_measure) || anyDuplicated(summary_measure) ||
      any(rows$period_basis != "ten_day") ||
      any(table(rows$release_period[product]) != 150L) ||
      any(table(rows$release_period[!product]) != 3L)) {
    .nbs_abort("nbs_manifest_error", "The ten-day history observation archive is invalid.")
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(rows$observation_date)
  rows$observation_period_start <- as.Date(rows$observation_period_start)
  rows$observation_period_end <- as.Date(rows$observation_period_end)
  assign("observations", rows, envir = .nbs_tenday_history_cache)
  rows
}

.nbs_tenday_history_observations_for_release <- function(period) {
  rows <- .nbs_load_tenday_history_observations()
  index <- .nbs_load_tenday_history_release_index()
  position <- match(period, index$period)
  if (is.na(position)) return(rows[FALSE, , drop = FALSE])
  first <- (position - 1L) * 153L + 1L
  rows[first:(first + 152L), , drop = FALSE]
}

.nbs_load_tenday_tuple_inceptions <- function() {
  if (exists("tuple_inceptions", envir = .nbs_tenday_history_cache,
      inherits = FALSE)) {
    return(get("tuple_inceptions", envir = .nbs_tenday_history_cache,
      inherits = FALSE))
  }
  rows <- utils::read.csv(
    .nbs_extdata_path("nbs-tenday-tuple-inceptions.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "policy_version", "dataset_id", "dimension_key", "product_code",
    "measure", "required_from", "first_release_period", "reason", "authority"
  )
  computed_keys <- vapply(seq_len(nrow(rows)), function(index) {
    nbs_dimension_key(list(
      product_code = rows$product_code[[index]], measure = rows$measure[[index]]
    ))
  }, character(1L))
  if (!identical(names(rows), required) || nrow(rows) != 330L ||
      any(rows$dataset_id != "NBS/CN/10D/PRODUCTION_MARKET_PRICES") ||
      anyDuplicated(rows$dimension_key) ||
      !identical(rows$dimension_key, computed_keys) ||
      anyNA(suppressWarnings(as.Date(rows$required_from))) ||
      any(rows$reason != "official_exact_source_tuple_inception") ||
      any(!grepl("^https://www\\.stats\\.gov\\.cn/", rows$authority))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The exact ten-day tuple-inception policy is invalid."
    )
  }
  rows$required_from <- as.Date(rows$required_from)
  assign("tuple_inceptions", rows, envir = .nbs_tenday_history_cache)
  rows
}
