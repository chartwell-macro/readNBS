# Retained NBS national industrial capacity-utilisation history.  This is kept
# separate from the detailed 2018 Q2+ industry snapshot: the historical annual
# chart and the early quarterly headlines do not publish that later panel.

.nbs_capacity_history_cache <- new.env(parent = emptyenv())

.nbs_load_capacity_history_release_index <- function() {
  if (exists("release_index", envir = .nbs_capacity_history_cache, inherits = FALSE)) {
    return(get("release_index", envir = .nbs_capacity_history_cache, inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-capacity-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "dataset_id", "period", "reference_period_start",
    "reference_period_end", "release_date", "source_url", "basket_version",
    "index_base", "expected_press_cells", "schema_version", "storage",
    "snapshot_sha256", "source_capture_sha256"
  )
  expected_periods <- c("2006-2022", "2017-Q4", "2018-Q1", "2019-Q1")
  expected_datasets <- c(
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY",
    rep("NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019", 3L)
  )
  if (!identical(names(index), required) || !identical(index$period, expected_periods) ||
      !identical(index$dataset_id, expected_datasets) || any(index$storage != "snapshot") ||
      any(!grepl("^[0-9a-f]{64}$", index$snapshot_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256))) {
    .nbs_abort("nbs_manifest_error", "The capacity-history release index is invalid.")
  }
  index$expected_press_cells <- suppressWarnings(as.integer(index$expected_press_cells))
  if (anyNA(index$expected_press_cells) || !identical(index$expected_press_cells, c(17L, 1L, 1L, 1L))) {
    .nbs_abort("nbs_manifest_error", "The capacity-history release cardinalities are invalid.")
  }
  source_index <- utils::read.csv(
    .nbs_extdata_path("nbs-capacity-history-source-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  source_required <- c("source_key", "source_url", "retained_file", "source_role", "source_capture_sha256")
  raw_dir <- .nbs_extdata_path("nbs-capacity-history")
  raw_paths <- file.path(raw_dir, source_index$retained_file)
  raw_hashes <- vapply(raw_paths, function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ), character(1L))
  if (!identical(names(source_index), source_required) || nrow(source_index) != 5L ||
      any(!file.exists(raw_paths)) ||
      !identical(unname(raw_hashes), source_index$source_capture_sha256) ||
      !all(index$source_capture_sha256 %in% source_index$source_capture_sha256)) {
    .nbs_abort("nbs_manifest_error", "A retained capacity-history source capture does not match its SHA-256.")
  }
  assign("release_index", index, envir = .nbs_capacity_history_cache)
  index
}

.nbs_load_capacity_history_observations <- function() {
  if (exists("observations", envir = .nbs_capacity_history_cache, inherits = FALSE)) {
    return(get("observations", envir = .nbs_capacity_history_cache, inherits = FALSE))
  }
  archive_path <- .nbs_extdata_path("nbs-capacity-history-observations.csv.gz")
  rows <- utils::read.csv(
    archive_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  index <- .nbs_load_capacity_history_release_index()
  required <- c(
    "dataset_id", "measure", "raw_value", "value", "observation_date", "series_id",
    "schema_version", "classification_id", "release_period", "release_source_url"
  )
  counts <- table(factor(rows$release_period, levels = index$period))
  actual_hash <- digest::digest(file = archive_path, algo = "sha256", serialize = FALSE)
  expected_hash <- unique(index$snapshot_sha256)
  annual <- rows$dataset_id == "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
  quarter <- rows$dataset_id == "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019"
  if (!all(required %in% names(rows)) || length(expected_hash) != 1L ||
      !identical(actual_hash, expected_hash) || nrow(rows) != 20L ||
      any(!is.finite(suppressWarnings(as.numeric(rows$value)))) ||
      anyDuplicated(paste(rows$series_id[annual], rows$observation_date[annual], sep = "\r")) ||
      anyDuplicated(paste(rows$series_id[quarter], rows$observation_date[quarter], sep = "\r")) ||
      !all(annual | quarter) ||
      !identical(as.integer(counts), index$expected_press_cells) ||
      any(rows$period_basis[annual] != "calendar_year") ||
      any(rows$period_basis[quarter] != "quarter") ||
      any(rows$seasonal_adjustment != "not_seasonally_adjusted")) {
    .nbs_abort("nbs_manifest_error", "The capacity-history observation archive is invalid.")
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(rows$observation_date)
  assign("observations", rows, envir = .nbs_capacity_history_cache)
  rows
}
