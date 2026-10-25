.nbs_load_press_series_manifest <- function() {
  manifest <- utils::read.csv(
    .nbs_extdata_path("nbs-press-series.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "series_id", "dataset_id", "release_id", "family",
    "title_en", "title_zh", "description_en", "description_zh", "frequency",
    "column_index", "default_start", "available_to", "aggregation_basis",
    "seasonal_adjustment", "derived_method", "source_vintage",
    "series_note_en", "series_note_zh", "unit_en", "unit_zh", "source_url",
    "release_date"
  )
  if (any(!required %in% names(manifest)) ||
      anyDuplicated(manifest$series_id)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS press-series manifest is invalid."
    )
  }
  manifest$column_index <- suppressWarnings(as.integer(manifest$column_index))
  if (anyNA(manifest$column_index) || any(manifest$column_index < 2L)) {
    .nbs_abort(
      "nbs_manifest_error",
      "Press-series column indexes must be positive table-value columns."
    )
  }
  manifest
}

.nbs_press_series_catalogue <- function(language) {
  manifest <- .nbs_load_press_series_manifest()
  data.frame(
    series_id = manifest$series_id,
    dataset_id = manifest$dataset_id,
    release_id = manifest$release_id,
    family = manifest$family,
    dimension_key = "",
    dimension_label = if (language == "en") "China" else "\u4e2d\u56fd",
    route = "press_revision_table",
    provider = "nbs",
    title = manifest[[paste0("title_", language)]],
    title_en = manifest$title_en,
    title_zh = manifest$title_zh,
    description = manifest[[paste0("description_", language)]],
    description_en = manifest$description_en,
    description_zh = manifest$description_zh,
    series_note = manifest[[paste0("series_note_", language)]],
    series_note_en = manifest$series_note_en,
    series_note_zh = manifest$series_note_zh,
    frequency = manifest$frequency,
    period_policy = "all_periods",
    aggregation_basis = manifest$aggregation_basis,
    seasonal_adjustment = manifest$seasonal_adjustment,
    derived_method = manifest$derived_method,
    source_vintage = manifest$source_vintage,
    unit = manifest[[paste0("unit_", language)]],
    unit_en = manifest$unit_en,
    unit_zh = manifest$unit_zh,
    geography = if (language == "en") "China" else "\u4e2d\u56fd",
    geography_code = "000000000000",
    available_from = as.Date(manifest$default_start),
    available_to = as.Date(manifest$available_to),
    manifest_version = manifest$manifest_version,
    source_url = manifest$source_url,
    source_note = "Official NBS press-release revision table",
    stringsAsFactors = FALSE
  )
}

.nbs_parse_activity_revision_table <- function(response, row, language) {
  document <- tryCatch(
    xml2::read_html(response$body_text),
    error = function(error) error
  )
  if (inherits(document, "error")) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The NBS activity revision release is not valid HTML.",
      parent = document
    )
  }
  tables <- rvest::html_table(document, fill = TRUE, header = FALSE)
  candidates <- Filter(
    function(table) nrow(table) == 14L && ncol(table) >= 4L,
    tables
  )
  if (length(candidates) != 1L) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf(
        "The NBS activity release contained %d candidate revision tables.",
        length(candidates)
      ),
      raw_sha256 = response$raw_sha256
    )
  }
  table <- as.data.frame(candidates[[1L]], stringsAsFactors = FALSE)
  month_labels <- trimws(as.character(table[[1L]][-1L]))
  values <- trimws(as.character(table[[row$column_index]][-1L]))
  if (length(month_labels) != 13L || length(values) != 13L) {
    .nbs_abort(
      "nbs_dataset_cardinality_error",
      "The official NBS activity revision table must contain 13 months.",
      expected_cells = 13L,
      actual_cells = length(values),
      raw_sha256 = response$raw_sha256
    )
  }
  years <- integer(length(month_labels))
  current_year <- NA_integer_
  dates <- as.Date(rep(NA_character_, length(month_labels)))
  for (index in seq_along(month_labels)) {
    label <- month_labels[[index]]
    year_match <- regmatches(label, regexpr("[0-9]{4}", label))
    if (length(year_match) && nzchar(year_match)) {
      current_year <- as.integer(year_match)
    }
    month_name <- trimws(sub(",?\\s*[0-9]{4}", "", label))
    month <- match(tolower(month_name), tolower(month.name))
    if (is.na(current_year) || is.na(month)) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Could not decode revision-table month `%s`.", label),
        raw_sha256 = response$raw_sha256
      )
    }
    dates[[index]] <- as.Date(sprintf("%04d-%02d-01", current_year, month))
  }
  numeric_values <- suppressWarnings(as.numeric(values))
  if (anyNA(numeric_values) || anyDuplicated(dates) ||
      !identical(dates, .nbs_period_sequence(min(dates), max(dates), "M"))) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The NBS activity revision table has invalid values or month order.",
      raw_sha256 = response$raw_sha256
    )
  }
  n <- length(dates)
  release_date <- as.Date(row$release_date)
  data.frame(
    series_id = rep(row$series_id, n),
    dataset_id = rep(row$dataset_id, n),
    release_id = rep(row$release_id, n),
    family = rep(row$family, n),
    dimension_key = rep("", n),
    dimension_label = rep(
      if (language == "en") "China" else "\u4e2d\u56fd", n
    ),
    route = rep("press_revision_table", n),
    provider = rep("nbs", n),
    title = rep(row[[paste0("title_", language)]], n),
    title_en = rep(row$title_en, n),
    title_zh = rep(row$title_zh, n),
    series_note = rep(row[[paste0("series_note_", language)]], n),
    series_note_en = rep(row$series_note_en, n),
    series_note_zh = rep(row$series_note_zh, n),
    date = dates,
    period = format(dates, "%Y-%m"),
    reference_date = dates,
    reference_period = format(dates, "%Y-%m"),
    reference_period_start = dates,
    reference_period_end = .nbs_next_period(dates, "M") - 1,
    span_months = rep(1L, n),
    aggregation_basis = rep("single_period", n),
    release_date = rep(release_date, n),
    vintage_date = rep(release_date, n),
    data_status = rep("official_revised_history_snapshot", n),
    seasonal_adjustment = rep("official_seasonally_adjusted", n),
    derived_method = rep("none", n),
    source_vintage = rep("first_release_press_archive", n),
    source_observation_periods = format(dates, "%Y-%m"),
    source_raw_sha256 = rep(response$raw_sha256, n),
    frequency = rep("M", n),
    value = numeric_values,
    raw_value = values,
    is_missing = rep(FALSE, n),
    unit = rep(row[[paste0("unit_", language)]], n),
    unit_en = rep(row$unit_en, n),
    unit_zh = rep(row$unit_zh, n),
    raw_unit = rep("%", n),
    scale = rep(1, n),
    geography_code = rep("000000000000", n),
    geography = rep(if (language == "en") "China" else "\u4e2d\u56fd", n),
    raw_geography = rep("China", n),
    catalog_uuid = rep(NA_character_, n),
    indicator_uuid = rep(NA_character_, n),
    root_uuid = rep(NA_character_, n),
    classification_id = rep("nbs-activity-sa-revision-table", n),
    stitch_policy = rep("pinned_press_revision_history", n),
    period_policy = rep("all_periods", n),
    slice_start = rep(as.Date(row$default_start), n),
    slice_end = rep(as.Date(row$available_to), n),
    retrieved_at = rep(response$retrieved_at, n),
    source_url = rep(row$source_url, n),
    source_note = rep("Official NBS press-release revision table", n),
    raw_sha256 = rep(response$raw_sha256, n),
    manifest_version = rep(row$manifest_version, n),
    parser_version = rep("2", n),
    package_version = rep(.nbs_package_version(), n),
    stringsAsFactors = FALSE
  )
}

.nbs_fetch_press_series <- function(
    series_id,
    start,
    end,
    language,
    client) {
  manifest <- .nbs_load_press_series_manifest()
  row <- manifest[manifest$series_id == series_id, , drop = FALSE]
  if (!nrow(row)) {
    return(NULL)
  }
  start_date <- if (is.null(start)) {
    as.Date(row$default_start)
  } else {
    .nbs_period_date(start, "M", "start")
  }
  end_date <- if (is.null(end)) {
    as.Date(row$available_to)
  } else {
    .nbs_period_date(end, "M", "end")
  }
  if (start_date > end_date) {
    .nbs_abort("nbs_period_error", "`start` is after `end`.")
  }
  archive_end <- as.Date("2025-05-01")
  archived <- .nbs_fetch_industry_sa_history(
    row, start_date, min(end_date, archive_end), language
  )
  if (is.null(archived)) {
    archived <- .nbs_fetch_fai_sa_history(
      row, start_date, min(end_date, archive_end), language
    )
  }
  if (is.null(archived)) {
    archived <- .nbs_fetch_retail_sa_history(
      row, start_date, min(end_date, archive_end), language
    )
  }
  if (!is.null(archived) && end_date <= archive_end) {
    rownames(archived) <- NULL
    return(archived)
  }
  retained_history_ids <- c(
    .nbs_industry_sa_history_id, .nbs_fai_sa_history_id,
    .nbs_retail_sa_history_id
  )
  current_start <- if (series_id %in% retained_history_ids) {
    max(start_date, archive_end + 1L)
  } else {
    start_date
  }
  response <- .nbs_request_document(
    client, row$source_url,
    operation = "NBS official activity revision table"
  )
  result <- .nbs_parse_activity_revision_table(response, row, language)
  result <- result[
    result$date >= current_start & result$date <= end_date, , drop = FALSE
  ]
  if (series_id %in% retained_history_ids) {
    result$source_vintage <- paste0(
      "official_nbs_revision_snapshot_", row$release_date
    )
  }
  if (!is.null(archived)) result <- rbind(archived, result)
  expected <- .nbs_period_sequence(start_date, end_date, "M")
  if (!identical(result$date, expected)) {
    .nbs_abort(
      "nbs_incomplete_period_error",
      "The pinned NBS revision archive does not cover the requested range.",
      series_id = series_id
    )
  }
  rownames(result) <- NULL
  result
}
