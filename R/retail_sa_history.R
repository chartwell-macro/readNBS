.nbs_retail_sa_history_id <-
  "NBS/CN/M/RETAIL/TOTAL/MOM_PERCENT_OFFICIAL_SA"

.nbs_load_retail_sa_history <- function() {
  path <- .nbs_extdata_path("nbs-retail-sa-history-selected.csv.gz")
  history <- utils::read.csv(
    gzfile(path), stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "dataset_id", "series_id", "observation_date",
    "value", "raw_value", "unit", "seasonal_adjustment", "data_status",
    "source_vintage", "vintage_date", "release_date", "release_source_url",
    "source_capture_sha256", "snapshot_id", "selection_rule"
  )
  if (any(!required %in% names(history)) || anyDuplicated(history$observation_date)) {
    .nbs_abort("nbs_manifest_error", "The retained NBS retail SA history is invalid.")
  }
  history$date <- as.Date(history$observation_date)
  history$value <- suppressWarnings(as.numeric(history$value))
  if (anyNA(history$date) || anyNA(history$value) ||
      !identical(history$date, .nbs_period_sequence(min(history$date), max(history$date), "M"))) {
    .nbs_abort("nbs_manifest_error", "The retained NBS retail SA history is incomplete.")
  }
  history
}

# Converts selected archival revision snapshots to the live activity-revision
# table observation contract. It makes no request and preserves each source
# vintage rather than assigning the date of a later current release.
.nbs_fetch_retail_sa_history <- function(row, start_date, end_date, language) {
  if (!identical(row$series_id[[1L]], .nbs_retail_sa_history_id)) return(NULL)
  history <- .nbs_load_retail_sa_history()
  history <- history[history$date >= start_date & history$date <= end_date, , drop = FALSE]
  if (!nrow(history)) return(NULL)
  n <- nrow(history)
  data.frame(
    series_id = rep(row$series_id, n), dataset_id = rep(row$dataset_id, n),
    release_id = rep(row$release_id, n), family = rep(row$family, n),
    dimension_key = rep("", n),
    dimension_label = rep(if (language == "en") "China" else "\u4E2D\u56FD", n),
    route = rep("press_revision_table_archive", n), provider = rep("nbs", n),
    title = rep(row[[paste0("title_", language)]], n), title_en = rep(row$title_en, n),
    title_zh = rep(row$title_zh, n), series_note = rep(row[[paste0("series_note_", language)]], n),
    series_note_en = rep(row$series_note_en, n), series_note_zh = rep(row$series_note_zh, n),
    date = history$date, period = format(history$date, "%Y-%m"), reference_date = history$date,
    reference_period = format(history$date, "%Y-%m"), reference_period_start = history$date,
    reference_period_end = .nbs_next_period(history$date, "M") - 1, span_months = rep(1L, n),
    aggregation_basis = rep("single_period", n), release_date = as.Date(history$release_date),
    vintage_date = as.Date(history$vintage_date), data_status = history$data_status,
    seasonal_adjustment = history$seasonal_adjustment, derived_method = rep("none", n),
    source_vintage = history$source_vintage, source_observation_periods = format(history$date, "%Y-%m"),
    source_raw_sha256 = history$source_capture_sha256, frequency = rep("M", n),
    value = history$value, raw_value = history$raw_value, is_missing = rep(FALSE, n),
    unit = rep(row[[paste0("unit_", language)]], n), unit_en = rep(row$unit_en, n),
    unit_zh = rep(row$unit_zh, n), raw_unit = rep("%", n), scale = rep(1, n),
    geography_code = rep("000000000000", n), geography = rep(if (language == "en") "China" else "\u4E2D\u56FD", n),
    raw_geography = rep("China", n), catalog_uuid = rep(NA_character_, n),
    indicator_uuid = rep(NA_character_, n), root_uuid = rep(NA_character_, n),
    classification_id = rep("nbs-activity-sa-revision-table", n),
    stitch_policy = rep("pinned_press_revision_history", n), period_policy = rep("all_periods", n),
    slice_start = rep(as.Date("2011-02-01"), n), slice_end = rep(as.Date("2025-05-01"), n),
    retrieved_at = rep(as.POSIXct(NA), n), source_url = history$release_source_url,
    source_note = rep("Retained official NBS press-release revision-table snapshot", n),
    raw_sha256 = history$source_capture_sha256, manifest_version = history$manifest_version,
    parser_version = rep("retail-sa-history-1", n), package_version = rep(.nbs_package_version(), n),
    stringsAsFactors = FALSE
  )
}
