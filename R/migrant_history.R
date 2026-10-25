# Source-faithful annual migrant-worker monitoring history.  This supplement
# is intentionally independent of the current detailed 2025 report: source
# tables and classifications changed repeatedly, so a release-cell selector is
# the only stable, truthful leaf identifier.

.nbs_migrant_history_cache <- new.env(parent = emptyenv())

.nbs_load_migrant_history_release_index <- function() {
  if (exists("release_index", envir = .nbs_migrant_history_cache,
      inherits = FALSE)) {
    return(get("release_index", envir = .nbs_migrant_history_cache,
      inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-migrant-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "dataset_id", "period", "reference_period_start",
    "reference_period_end", "release_date", "source_url", "basket_version",
    "index_base", "expected_press_cells", "schema_version", "storage",
    "snapshot_sha256", "source_capture_sha256"
  )
  valid_periods <- as.character(2012:2024)
  if (!identical(names(index), required) || !identical(index$period, valid_periods) ||
      any(index$dataset_id != "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024") ||
      any(index$storage != "snapshot") ||
      any(!grepl("^[0-9a-f]{64}$", index$snapshot_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256)) ||
      anyDuplicated(index$period)) {
    .nbs_abort("nbs_manifest_error", "The migrant-worker history release index is invalid.")
  }
  index$expected_press_cells <- suppressWarnings(as.integer(index$expected_press_cells))
  if (anyNA(index$expected_press_cells) || any(index$expected_press_cells < 1L)) {
    .nbs_abort("nbs_manifest_error", "The migrant-worker history has invalid release cardinalities.")
  }
  raw_dir <- .nbs_extdata_path("nbs-migrant-history")
  raw_paths <- file.path(raw_dir, paste0(index$period, ".html"))
  raw_hashes <- vapply(raw_paths, function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ), character(1L))
  if (any(!file.exists(raw_paths)) || !identical(unname(raw_hashes), index$source_capture_sha256)) {
    .nbs_abort("nbs_manifest_error", "A retained migrant-worker history source capture does not match its SHA-256.")
  }
  assign("release_index", index, envir = .nbs_migrant_history_cache)
  index
}

.nbs_load_migrant_history_observations <- function() {
  if (exists("observations", envir = .nbs_migrant_history_cache,
      inherits = FALSE)) {
    return(get("observations", envir = .nbs_migrant_history_cache,
      inherits = FALSE))
  }
  path <- .nbs_extdata_path("nbs-migrant-history-observations.csv.gz")
  rows <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  index <- .nbs_load_migrant_history_release_index()
  expected_hash <- unique(index$snapshot_sha256)
  actual_hash <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  required <- c(
    "dataset_id", "release_period", "measure", "raw_value", "value",
    "observation_date", "schema_version", "classification_id",
    "release_source_url", "source_capture_sha256", "table_index", "row_index",
    "column_index"
  )
  counts <- table(factor(rows$release_period, levels = index$period))
  if (!nrow(rows) || any(!required %in% names(rows)) ||
      length(expected_hash) != 1L || !identical(actual_hash, expected_hash) ||
      any(rows$dataset_id != index$dataset_id[[1L]]) ||
      any(!is.finite(suppressWarnings(as.numeric(rows$value)))) ||
      anyDuplicated(rows$measure) ||
      !identical(as.integer(counts), index$expected_press_cells)) {
    .nbs_abort("nbs_manifest_error", "The migrant-worker history observation archive is invalid.")
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(rows$observation_date)
  for (column in c("table_index", "row_index", "column_index")) {
    rows[[column]] <- as.integer(rows[[column]])
  }
  assign("observations", rows, envir = .nbs_migrant_history_cache)
  rows
}
