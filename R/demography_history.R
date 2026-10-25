# Frozen early demographic communiques are kept outside the large press
# archive.  The 2010 release is a preliminary headline only, while 2011 adds
# a population table.  Keeping this small, schema-bounded supplement separate
# prevents either release from being silently treated as the later five-field
# communique panel.

.nbs_demography_history_cache <- new.env(parent = emptyenv())

.nbs_load_demography_history_release_index <- function() {
  if (exists("release_index", envir = .nbs_demography_history_cache,
      inherits = FALSE)) {
    return(get("release_index", envir = .nbs_demography_history_cache,
      inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-demography-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "dataset_id", "period", "reference_period_start",
    "reference_period_end", "release_date", "source_url", "basket_version",
    "index_base", "expected_press_cells", "schema_version", "storage",
    "snapshot_sha256", "source_capture_sha256"
  )
  if (!nrow(index) || any(!required %in% names(index)) ||
      anyDuplicated(paste(index$dataset_id, index$period, sep = "\r")) ||
      any(index$storage != "snapshot") ||
      any(!grepl("^[0-9a-f]{64}$", index$snapshot_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The early-demography release index is invalid."
    )
  }
  index$expected_press_cells <- suppressWarnings(as.integer(
    index$expected_press_cells
  ))
  if (anyNA(index$expected_press_cells) ||
      !identical(index$expected_press_cells, c(1L, 6L))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The early-demography release index has invalid cardinalities."
    )
  }
  assign("release_index", index, envir = .nbs_demography_history_cache)
  index
}

.nbs_load_demography_history_observations <- function() {
  if (exists("observations", envir = .nbs_demography_history_cache,
      inherits = FALSE)) {
    return(get("observations", envir = .nbs_demography_history_cache,
      inherits = FALSE))
  }
  path <- .nbs_extdata_path("nbs-demography-history-observations.csv.gz")
  rows <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "dataset_id", "release_period", "measure", "period_basis", "raw_value",
    "value", "observation_date", "schema_version", "basket_version",
    "release_source_url"
  )
  expected <- unique(.nbs_load_demography_history_release_index()$snapshot_sha256)
  actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  required_measures <- c(
    "total_population_year_end", "total_population_year_end", "births_level", "deaths_level",
    "urbanisation_percent", "age_15_59_year_end", "age_60_plus_year_end"
  )
  if (!nrow(rows) || any(!required %in% names(rows)) ||
      length(expected) != 1L || !identical(actual, expected) ||
      any(!is.finite(suppressWarnings(as.numeric(rows$value)))) ||
      !identical(rows$release_period, c("2010", rep("2011", 6L))) ||
      !identical(rows$measure, required_measures)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The early-demography observation archive is invalid."
    )
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(ifelse(
    nzchar(rows$observation_date), rows$observation_date, NA_character_
  ))
  assign("observations", rows, envir = .nbs_demography_history_cache)
  rows
}
