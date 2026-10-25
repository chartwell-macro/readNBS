`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.nbs_package_version <- function() {
  as.character(utils::packageVersion("readNBS"))
}

.nbs_scalar_character <- function(x, name, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .nbs_abort(
      "nbs_argument_error",
      sprintf("`%s` must be one non-missing character value.", name)
    )
  }
  x <- trimws(x)
  if (!allow_empty && !nzchar(x)) {
    .nbs_abort(
      "nbs_argument_error",
      sprintf("`%s` must not be empty.", name)
    )
  }
  x
}

.nbs_scalar_number <- function(x, name, lower, upper, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .nbs_abort(
      "nbs_argument_error",
      sprintf("`%s` must be one finite number.", name)
    )
  }
  if (x < lower || x > upper || (integer && x != as.integer(x))) {
    .nbs_abort(
      "nbs_argument_error",
      sprintf("`%s` must be between %s and %s%s.", name, lower, upper,
              if (integer) " and integer-valued" else "")
    )
  }
  if (integer) as.integer(x) else x
}

.nbs_as_character_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(NA_character_)
  }
  as.character(x[[1L]])
}

.nbs_first_character <- function(...) {
  candidates <- list(...)
  for (candidate in candidates) {
    value <- .nbs_as_character_scalar(candidate)
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NA_character_
}

.nbs_as_logical_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(NA)
  }
  isTRUE(x[[1L]])
}

.nbs_raw_body <- function(x) {
  if (is.raw(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(charToRaw(enc2utf8(x)))
  }
  .nbs_abort(
    "nbs_transport_contract_error",
    "The transport response `body` must be a raw vector or one character value."
  )
}

.nbs_header <- function(headers, name) {
  if (is.null(headers) || !length(headers)) {
    return(NA_character_)
  }
  header_names <- names(headers)
  if (is.null(header_names)) {
    return(NA_character_)
  }
  hit <- which(tolower(header_names) == tolower(name))
  if (!length(hit)) {
    return(NA_character_)
  }
  as.character(headers[[hit[[1L]]]])[[1L]]
}

.nbs_sha256 <- function(raw_body) {
  digest::digest(raw_body, algo = "sha256", serialize = FALSE)
}

.nbs_empty_observations <- function() {
  data.frame(
    series_id = character(),
    dataset_id = character(),
    release_id = character(),
    family = character(),
    dimension_key = character(),
    dimension_label = character(),
    route = character(),
    provider = character(),
    title = character(),
    title_en = character(),
    title_zh = character(),
    series_note = character(),
    series_note_en = character(),
    series_note_zh = character(),
    date = as.Date(character()),
    period = character(),
    reference_date = as.Date(character()),
    reference_period = character(),
    reference_period_start = as.Date(character()),
    reference_period_end = as.Date(character()),
    span_months = integer(),
    aggregation_basis = character(),
    release_date = as.Date(character()),
    vintage_date = as.Date(character()),
    data_status = character(),
    seasonal_adjustment = character(),
    derived_method = character(),
    source_vintage = character(),
    source_observation_periods = character(),
    source_raw_sha256 = character(),
    frequency = character(),
    value = numeric(),
    raw_value = character(),
    is_missing = logical(),
    unit = character(),
    unit_en = character(),
    unit_zh = character(),
    raw_unit = character(),
    scale = numeric(),
    geography_code = character(),
    geography = character(),
    raw_geography = character(),
    catalog_uuid = character(),
    indicator_uuid = character(),
    root_uuid = character(),
    classification_id = character(),
    stitch_policy = character(),
    period_policy = character(),
    slice_start = as.Date(character()),
    slice_end = as.Date(character()),
    retrieved_at = as.POSIXct(character(), tz = "UTC"),
    source_url = character(),
    source_note = character(),
    raw_sha256 = character(),
    manifest_version = character(),
    parser_version = character(),
    package_version = character(),
    stringsAsFactors = FALSE
  )
}
