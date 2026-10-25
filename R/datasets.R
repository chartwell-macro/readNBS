.nbs_dataset_manifest_columns <- c(
  "manifest_version", "dataset_id", "release_id", "family", "title_en",
  "title_zh", "description_en", "description_zh", "frequency", "route",
  "default_start", "available_to", "coverage_en", "coverage_zh",
  "required_dimensions", "panel_allowed", "expected_rows", "expected_cells",
  "period_policy",
  "aggregation_basis", "seasonal_adjustment", "derived_method",
  "source_vintage", "series_note_en", "series_note_zh", "unit_en", "unit_zh",
  "source_url", "source_note"
)

.nbs_continuous_wrapper_dataset_ids <- c(
  "NBS/CN/A/WAGES/LONGITUDINAL",
  "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL",
  "NBS/CN/A/DEMOGRAPHY/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_OUTPUT/KEY_PRODUCTS",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL"
)

.nbs_both_history_dataset_ids <- c(
  "NBS/CN/M/HOUSING_PRICES/70_CITY",
  "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION",
  "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
  "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION",
  "NBS/CN/M/RETAIL_SALES/DETAIL",
  "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
  "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION",
  "NBS/CN/M/CPI/PRICE_DETAIL",
  "NBS/CN/M/PPI/PRICE_DETAIL",
  "NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING",
  "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
)

.nbs_exact_source_dataset_ids <- c(
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY",
  "NBS/CN/A/EMPLOYMENT_WAGES",
  "NBS/CN/A/MIGRANT_WORKERS",
  "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
  "NBS/CN/A/YEARBOOK_DEMOGRAPHY_LEGACY",
  "NBS/CN/A/YEARBOOK_WAGES_URBAN_UNITS_LEGACY",
  "NBS/CN/A/YEARBOOK_PROPERTY_DEVELOPMENT_LEGACY",
  "NBS/CN/A/YEARBOOK_HOUSEHOLDS_URBAN_RURAL_LEGACY",
  "NBS/CN/A/YEARBOOK_INDUSTRIAL_BENEFITS_LEGACY",
  "NBS/CN/A/YEARBOOK_RETAIL_REGIONAL_LEGACY",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017",
  "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY",
  "NBS/CN/A/WAGES/PRIVATE_HISTORY",
  "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
  "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012",
  "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024",
  "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019"
)

.nbs_continuous_dataset_replacements <- c(
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL",
  "NBS/CN/A/EMPLOYMENT_WAGES" = "NBS/CN/A/WAGES/LONGITUDINAL",
  "NBS/CN/A/MIGRANT_WORKERS" =
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL",
  "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017" =
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL",
  "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY" = "NBS/CN/A/WAGES/LONGITUDINAL",
  "NBS/CN/A/WAGES/PRIVATE_HISTORY" = "NBS/CN/A/WAGES/LONGITUDINAL",
  "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024" =
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL",
  "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019" =
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
)

.nbs_dataset_history_contract <- function(dataset_ids) {
  role <- rep(NA_character_, length(dataset_ids))
  role[dataset_ids %in% .nbs_both_history_dataset_ids] <- "both"
  role[dataset_ids %in% .nbs_continuous_wrapper_dataset_ids] <- "continuous"
  role[dataset_ids %in% .nbs_exact_source_dataset_ids] <- "exact_source"
  if (anyNA(role)) {
    .nbs_abort(
      "nbs_manifest_error",
      "An NBS dataset is missing from the history-view contract."
    )
  }
  continuous_dataset_id <- ifelse(role == "exact_source", "", dataset_ids)
  replacements <- unname(.nbs_continuous_dataset_replacements[dataset_ids])
  replacement_rows <- role == "exact_source" & !is.na(replacements)
  continuous_dataset_id[replacement_rows] <- replacements[replacement_rows]
  data.frame(
    history_role = role,
    continuous_dataset_id = continuous_dataset_id,
    stringsAsFactors = FALSE
  )
}

.nbs_extdata_path <- function(filename) {
  path <- system.file("extdata", filename, package = "readNBS", mustWork = FALSE)
  if (!nzchar(path)) {
    candidate <- file.path("inst", "extdata", filename)
    if (file.exists(candidate)) {
      path <- candidate
    }
  }
  if (!nzchar(path) || !file.exists(path)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("The bundled `%s` file could not be found.", filename)
    )
  }
  path
}

.nbs_dataset_metadata_cache <- new.env(parent = emptyenv())

.nbs_sentence_case <- function(value) {
  if (!nzchar(value)) return(value)
  paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
}

.nbs_humanise_dimension_label_en <- function(
    dataset_id, dimension, value, label) {
  label <- trimws(label)

  fixed_labels <- c(
    "ytd" = "Year to date",
    "current" = "Current month",
    "end_period" = "End of period",
    "not_applicable" = "Not applicable"
  )
  if (value %in% names(fixed_labels) &&
      dimension %in% c("period_basis", "specification")) {
    return(unname(fixed_labels[[value]]))
  }

  # A few generated measure labels mirror their stable machine value. The
  # value remains the selector contract; the label is deliberately phrased as
  # chart text and carries the comparison as context rather than an acronym.
  machine_measure <- identical(
    tolower(label), gsub("_", " ", tolower(value), fixed = TRUE)
  )
  if (dimension == "measure" && machine_measure) {
    if (grepl("_level$", value)) {
      label <- sub("_level$", "", value)
    } else if (grepl("_yoy(_percent)?$", value)) {
      label <- paste0(
        sub("_yoy(_percent)?$", "", value),
        " \u2014 year-on-year change"
      )
    }
  }

  machine_path <- grepl("/", label, fixed = TRUE) &&
    grepl("_", label, fixed = TRUE) &&
    grepl("^[[:upper:][:digit:]_/ -]+$", label)
  machine_text <- machine_path || grepl("_", label, fixed = TRUE) ||
    identical(label, tolower(label))
  label <- gsub("_", " ", label, fixed = TRUE)
  if (machine_path) label <- gsub("/", " \u2014 ", label, fixed = TRUE)
  if (machine_text) label <- tolower(label)

  capacity <- grepl(
    "INDUSTRIAL_CAPACITY_UTILISATION", dataset_id, fixed = TRUE
  )
  replacements <- c(
    "prior year ytd level" = "prior-year year-to-date value",
    "current yoy change pp" = if (capacity) {
      "current-quarter year-on-year change (percentage points)"
    } else {
      "current-period year-on-year change (percentage points)"
    },
    "ytd yoy change pp" =
      "year-to-date year-on-year change (percentage points)",
    "ytd real yoy percent" = "year-to-date real year-on-year change",
    "current yoy percent" = "current-period year-on-year change",
    "ytd yoy percent" = "year-to-date year-on-year change",
    "mom percent official sa" =
      "month-on-month change (official seasonally adjusted)",
    "ytd level" = "year to date",
    "end level" = "end of period",
    "current level" = "current period",
    "current percent" = if (capacity) {
      "current-quarter utilisation rate"
    } else {
      "current period"
    },
    "ytd percent" = if (capacity) {
      "year-to-date utilisation rate"
    } else {
      "year to date"
    },
    "yoy percent" = "year-on-year change",
    "mom percent" = "month-on-month change"
  )
  for (from in names(replacements)) {
    label <- gsub(
      paste0("\\b", from, "\\b"), replacements[[from]], label,
      ignore.case = TRUE, perl = TRUE
    )
  }
  label <- gsub(
    "\\byoy\\b", "year-on-year change", label,
    ignore.case = TRUE, perl = TRUE
  )
  label <- gsub(
    "\\bytd\\b", "year to date", label,
    ignore.case = TRUE, perl = TRUE
  )
  label <- gsub(
    "\\bmom\\b", "month-on-month change", label,
    ignore.case = TRUE, perl = TRUE
  )
  label <- gsub(
    "\\bpp\\b", "percentage points", label,
    ignore.case = TRUE, perl = TRUE
  )
  if (grepl("_change_pp$", value, ignore.case = TRUE) &&
      !grepl("percentage points", label, ignore.case = TRUE)) {
    label <- paste0(label, " (percentage points)")
  }
  label <- gsub("[[:space:]]+", " ", trimws(label), perl = TRUE)
  .nbs_sentence_case(label)
}

.nbs_dataset_member_labels <- function(dimensions, filters, language) {
  label_column <- paste0("label_", language)
  vapply(names(filters), function(name) {
    member <- dimensions[
      dimensions$dimension == name &
        dimensions$value == filters[[name]], , drop = FALSE
    ]
    if (nrow(member)) member[[label_column]][[1L]] else filters[[name]]
  }, character(1L))
}

.nbs_dataset_display_title <- function(dataset, dimensions, filters, language) {
  labels <- .nbs_dataset_member_labels(dimensions, filters, language)
  separator <- if (language == "en") " \u2014 " else "\u2014"
  paste(
    dataset[[paste0("title_", language)]][[1L]],
    paste(labels, collapse = separator),
    sep = separator
  )
}

.nbs_load_dataset_manifest <- function() {
  if (exists("manifest", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE)) {
    return(get("manifest", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE))
  }
  manifest <- utils::read.csv(
    .nbs_extdata_path("nbs-dataset-manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  yearbook_manifest <- utils::read.csv(
    .nbs_extdata_path("nbs-yearbook-dataset-manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  manifest <- rbind(manifest, yearbook_manifest[names(manifest)])
  supplements <- c(
    "nbs-longitudinal-dataset-manifest.csv",
    "nbs-demography-longitudinal-dataset-manifest.csv",
    "nbs-key-output-dataset-manifest.csv",
    "nbs-industrial-dataset-manifest.csv",
    "nbs-industrial-longitudinal-dataset-manifest.csv",
    "nbs-retail-dataset-manifest.csv",
    "nbs-wages-history-dataset-manifest.csv",
    "nbs-households-legacy-dataset-manifest.csv",
    "nbs-migrant-history-dataset-manifest.csv",
    "nbs-capacity-history-dataset-manifest.csv"
  )
  for (filename in supplements) {
    supplement <- utils::read.csv(
      .nbs_extdata_path(filename),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    manifest <- rbind(manifest, supplement)
  }
  missing <- setdiff(.nbs_dataset_manifest_columns, names(manifest))
  if (length(missing)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "The NBS dataset manifest is missing required columns: %s.",
        paste(missing, collapse = ", ")
      )
    )
  }
  if (!nrow(manifest) || anyDuplicated(manifest$dataset_id)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The NBS dataset manifest must contain unique dataset identifiers."
    )
  }
  history_ids <- c(
    .nbs_both_history_dataset_ids,
    .nbs_continuous_wrapper_dataset_ids,
    .nbs_exact_source_dataset_ids
  )
  if (anyDuplicated(history_ids) ||
      !setequal(history_ids, manifest$dataset_id)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The NBS dataset history-view contract is incomplete or overlapping."
    )
  }
  replacement_sources <- names(.nbs_continuous_dataset_replacements)
  replacement_targets <- unname(.nbs_continuous_dataset_replacements)
  if (any(!replacement_sources %in% .nbs_exact_source_dataset_ids) ||
      any(!replacement_targets %in% manifest$dataset_id) ||
      any(replacement_targets %in% .nbs_exact_source_dataset_ids)) {
    .nbs_abort(
      "nbs_manifest_error",
      "An NBS exact-source dataset has an invalid continuous replacement."
    )
  }
  required <- setdiff(
    .nbs_dataset_manifest_columns,
    c("available_to")
  )
  for (column in required) {
    if (any(!nzchar(trimws(manifest[[column]])))) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf("Dataset manifest column `%s` contains an empty value.", column)
      )
    }
  }
  if (any(!manifest$frequency %in% c("10D", "M", "Q", "A"))) {
    .nbs_abort(
      "nbs_manifest_error",
      "Dataset frequencies must be one of 10D, M, Q or A."
    )
  }
  if (any(!manifest$route %in% c(
    "portal_uuid_panel", "portal_uuid", "portal_uuid_tree_plus_press",
    "portal_uuid_plus_press", "press_release_table", "press_html_table",
    "retained_longitudinal_series", "yearbook_snapshot"
  ))) {
    .nbs_abort(
      "nbs_manifest_error",
      "A dataset manifest row has an unsupported route."
    )
  }
  manifest$panel_allowed <- vapply(
    tolower(manifest$panel_allowed),
    function(value) {
      if (!value %in% c("true", "false")) {
        .nbs_abort(
          "nbs_manifest_error",
          "Dataset `panel_allowed` values must be TRUE or FALSE."
        )
      }
      identical(value, "true")
    },
    logical(1)
  )
  manifest$expected_rows <- suppressWarnings(as.integer(manifest$expected_rows))
  manifest$expected_cells <- suppressWarnings(as.integer(manifest$expected_cells))
  if (anyNA(manifest$expected_rows) || any(manifest$expected_rows < 1L) ||
      anyNA(manifest$expected_cells) || any(manifest$expected_cells < 1L)) {
    .nbs_abort(
      "nbs_manifest_error",
      "Dataset cardinalities must be positive integers."
    )
  }
  starts <- suppressWarnings(as.Date(manifest$default_start))
  ends <- suppressWarnings(as.Date(ifelse(
    nzchar(manifest$available_to), manifest$available_to, "9999-12-31"
  )))
  if (anyNA(starts) || anyNA(ends) || any(starts > ends)) {
    .nbs_abort(
      "nbs_manifest_error",
      "Dataset manifest date bounds must be valid ISO dates."
    )
  }
  assign("manifest", manifest, envir = .nbs_dataset_metadata_cache)
  manifest
}

.nbs_load_dataset_dimensions <- function() {
  if (exists("dimensions", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE)) {
    return(get("dimensions", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE))
  }
  dimensions <- utils::read.csv(
    .nbs_extdata_path("nbs-dataset-dimensions.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  yearbook_dimensions <- utils::read.csv(
    .nbs_extdata_path("nbs-yearbook-dataset-dimensions.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  dimensions <- rbind(dimensions, yearbook_dimensions[names(dimensions)])
  supplements <- c(
    "nbs-longitudinal-dataset-dimensions.csv",
    "nbs-demography-longitudinal-dataset-dimensions.csv",
    "nbs-key-output-dataset-dimensions.csv",
    "nbs-industrial-dataset-dimensions.csv.gz",
    "nbs-industrial-longitudinal-dataset-dimensions.csv.gz",
    "nbs-retail-dataset-dimensions.csv",
    "nbs-wages-history-dataset-dimensions.csv",
    "nbs-households-legacy-dataset-dimensions.csv",
    "nbs-migrant-history-dataset-dimensions.csv",
    "nbs-capacity-history-dataset-dimensions.csv",
    "nbs-tenday-history-dataset-dimensions.csv"
  )
  for (filename in supplements) {
    supplement <- utils::read.csv(
      .nbs_extdata_path(filename),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    dimensions <- rbind(dimensions, supplement)
  }
  required <- c(
    "manifest_version", "dataset_id", "dimension", "value", "label_en",
    "label_zh", "route_value", "sort_order", "valid_from", "valid_to"
  )
  missing <- setdiff(required, names(dimensions))
  if (length(missing)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "The NBS dataset-dimension manifest is missing: %s.",
        paste(missing, collapse = ", ")
      )
    )
  }
  known <- .nbs_load_dataset_manifest()$dataset_id
  if (any(!dimensions$dataset_id %in% known)) {
    .nbs_abort(
      "nbs_manifest_error",
      "A dataset dimension refers to an unknown dataset."
    )
  }
  dimensions$sort_order <- suppressWarnings(as.integer(dimensions$sort_order))
  if (anyNA(dimensions$sort_order) || any(dimensions$sort_order < 1L)) {
    .nbs_abort(
      "nbs_manifest_error",
      "Dataset dimension sort orders must be positive integers."
    )
  }
  if (anyDuplicated(paste(
    dimensions$dataset_id, dimensions$dimension, dimensions$value,
    sep = "\r"
  ))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The dataset-dimension manifest contains duplicate values."
    )
  }
  assign("dimensions", dimensions, envir = .nbs_dataset_metadata_cache)
  dimensions
}

#' List dimensional NBS datasets
#'
#' This catalogue is the entry point for official dimensional panels,
#' release tables, retained yearbook snapshots, and longitudinal archives. It
#' is bundled, deterministic, and never performs a network request. A dataset
#' describes the dimensions and publication geometry of a source; scalar
#' portal series remain available from [nbs_catalogue()].
#'
#' @param language Display language, either `"en"` or `"zh"`.
#' @return A base data frame with one row per stable dataset and these 36
#' columns, in this order: `dataset_id`, `release_id`, `family`,
#' `dimension_key`, `dimension_label`, `route`, `provider`, `title`, `title_en`,
#' `title_zh`, `description`, `description_en`, `description_zh`, `frequency`,
#' `period_policy`, `required_dimensions`, `panel_allowed`, `expected_rows`,
#' `expected_cells`, `aggregation_basis`, `seasonal_adjustment`,
#' `derived_method`, `source_vintage`, `series_note`, `series_note_en`,
#' `series_note_zh`, `unit`, `unit_en`, `unit_zh`, `available_from`,
#' `available_to`, `manifest_version`, `source_url`, `source_note`,
#' `history_role`, and `continuous_dataset_id`.
#' All columns are character except logical `panel_allowed`, integer
#' `expected_rows` and `expected_cells`, and `Date` `available_from` and
#' `available_to`. The selected `language` populates the unsuffixed labels,
#' description, notes, unit, and dimension label; bilingual columns remain
#' available. `available_to` is `NA` for an open-ended portal dataset.
#'
#' @details The result is derived from versioned package data and cached only
#' by the lower-level metadata loaders. It does not read the scalar catalogue,
#' call the NBS portal, write files, or require credentials. `history_role`
#' records whether a dataset is both a continuous portal view and an exact
#' source view, a continuous view, or an exact source view; when a historical
#' dataset has a preferred continuous replacement, that ID is in
#' `continuous_dataset_id`. Malformed package data raises `nbs_manifest_error`.
#' `language` is checked with [match.arg()].
#'
#' @seealso [nbs_dataset_dimensions()], [nbs_dataset_valid_tuples()],
#' [nbs_fetch_dataset()], [nbs_catalogue()]
#'
#' @examples
#' datasets <- nbs_dataset_catalogue(language = "en")
#' datasets[1, c("dataset_id", "title", "frequency", "route", "required_dimensions")]
#' @export
nbs_dataset_catalogue <- function(language = c("en", "zh")) {
  language <- match.arg(language)
  manifest <- .nbs_load_dataset_manifest()
  available_from <- as.Date(manifest$default_start)
  available_to <- as.Date(ifelse(
    nzchar(manifest$available_to), manifest$available_to, NA_character_
  ))
  releases <- tryCatch({
    columns <- c("dataset_id", "reference_period_start", "reference_period_end")
    rbind(
      .nbs_load_release_index()[columns],
      .nbs_load_yearbook_release_index()[columns]
    )
  }, error = function(error) NULL)
  if (!is.null(releases)) {
    for (index in seq_len(nrow(manifest))) {
      members <- releases$dataset_id == manifest$dataset_id[[index]]
      if (!any(members)) next
      available_from[[index]] <- min(
        available_from[[index]],
        as.Date(releases$reference_period_start[members])
      )
      if (!grepl("portal_uuid", manifest$route[[index]], fixed = TRUE)) {
        available_to[[index]] <- max(
          as.Date(releases$reference_period_end[members])
        )
      }
    }
  }
  # `default_start` is the reviewed earliest observation (including a
  # prior-year value printed in a later release and the earliest exact portal
  # leaf used by a dimensional panel). Snapshot release indexes can extend
  # that bound without loading the much larger scalar catalogue merely to
  # build the small dataset search index.
  history <- .nbs_dataset_history_contract(manifest$dataset_id)
  data.frame(
    dataset_id = manifest$dataset_id,
    release_id = manifest$release_id,
    family = manifest$family,
    dimension_key = "",
    dimension_label = manifest[[paste0("coverage_", language)]],
    route = manifest$route,
    provider = "nbs",
    title = manifest[[paste0("title_", language)]],
    title_en = manifest$title_en,
    title_zh = manifest$title_zh,
    description = manifest[[paste0("description_", language)]],
    description_en = manifest$description_en,
    description_zh = manifest$description_zh,
    frequency = manifest$frequency,
    period_policy = manifest$period_policy,
    required_dimensions = manifest$required_dimensions,
    panel_allowed = manifest$panel_allowed,
    expected_rows = manifest$expected_rows,
    expected_cells = manifest$expected_cells,
    aggregation_basis = manifest$aggregation_basis,
    seasonal_adjustment = manifest$seasonal_adjustment,
    derived_method = manifest$derived_method,
    source_vintage = manifest$source_vintage,
    series_note = manifest[[paste0("series_note_", language)]],
    series_note_en = manifest$series_note_en,
    series_note_zh = manifest$series_note_zh,
    unit = manifest[[paste0("unit_", language)]],
    unit_en = manifest$unit_en,
    unit_zh = manifest$unit_zh,
    available_from = available_from,
    available_to = available_to,
    manifest_version = manifest$manifest_version,
    source_url = manifest$source_url,
    source_note = manifest$source_note,
    history_role = history$history_role,
    continuous_dataset_id = history$continuous_dataset_id,
    stringsAsFactors = FALSE
  )
}

#' List the dimensions of bundled NBS datasets
#'
#' Dimension members are versioned and include their transport value, display
#' labels, validity window, and (where available) member-specific unit and
#' semantics. Use the returned `value` fields in `filters` for
#' [nbs_resolve_dataset_series()] or [nbs_fetch_dataset()].
#'
#' @param dataset_id Optional stable dataset ID. When `NULL`, dimensions for
#'   every dataset are returned.
#' @param language Display language, either `"en"` or `"zh"`.
#' @return A data frame with these 14 columns, in this order:
#' `manifest_version`, `dataset_id`, `dimension`, `value`, `label_en`,
#' `label_zh`, `route_value`, `sort_order`, `valid_from`, `valid_to`,
#' `unit_en`, `unit_zh`, `semantics`, and `label`.
#' All columns are character except integer `sort_order`. `valid_from` and
#' `valid_to` are ISO-8601 character dates because they are manifest values,
#' not observation dates; an empty `valid_to` denotes an open-ended member
#' validity interval. `label` is the selected language's display label;
#' `label_en` is humanized for presentation while `value` and `route_value`
#' remain stable machine selectors. Rows are ordered by dataset, dimension,
#' and manifest `sort_order`.
#'
#' @details With `dataset_id = NULL`, all bundled datasets are returned.
#' Otherwise the ID must occur in [nbs_dataset_catalogue()]. This function is
#' local and deterministic, performs no network request, writes no files, and
#' keeps only an in-memory metadata cache. Invalid arguments raise
#' `nbs_argument_error`; an unknown ID raises `nbs_unknown_dataset_error`; a
#' malformed member manifest raises `nbs_manifest_error`. `language` is
#' checked with [match.arg()].
#'
#' @seealso [nbs_dataset_catalogue()], [nbs_dataset_valid_tuples()],
#' [nbs_resolve_dataset_series()]
#'
#' @examples
#' nbs_dataset_dimensions(
#'   "NBS/CN/M/CPI/PRICE_DETAIL", language = "en"
#' )[1:3, c("dimension", "value", "label", "unit_en")]
#' @export
nbs_dataset_dimensions <- function(
    dataset_id = NULL,
    language = c("en", "zh")) {
  language <- match.arg(language)
  if (!is.null(dataset_id)) {
    dataset_id <- .nbs_scalar_character(dataset_id, "dataset_id")
    if (!dataset_id %in% .nbs_load_dataset_manifest()$dataset_id) {
      .nbs_abort(
        "nbs_unknown_dataset_error",
        sprintf("Unknown NBS dataset `%s`.", dataset_id),
        dataset_id = dataset_id
      )
    }
  }
  cache_key <- paste0(
    "dimensions_display\r",
    if (is.null(dataset_id)) "*" else dataset_id
  )
  if (exists(
      cache_key,
      envir = .nbs_dataset_metadata_cache,
      inherits = FALSE
  )) {
    dimensions <- get(
      cache_key,
      envir = .nbs_dataset_metadata_cache,
      inherits = FALSE
    )
  } else {
    dimensions <- .nbs_load_dataset_dimensions()
    if (!is.null(dataset_id)) {
      dimensions <- dimensions[
        dimensions$dataset_id == dataset_id, , drop = FALSE
      ]
    }
    dimensions$label_en <- mapply(
      .nbs_humanise_dimension_label_en,
      dimensions$dataset_id,
      dimensions$dimension,
      dimensions$value,
      dimensions$label_en,
      USE.NAMES = FALSE
    )
    dimensions <- dimensions[
      order(
        dimensions$dataset_id,
        dimensions$dimension,
        dimensions$sort_order
      ),
      ,
      drop = FALSE
    ]
    rownames(dimensions) <- NULL
    assign(
      cache_key,
      dimensions,
      envir = .nbs_dataset_metadata_cache
    )
  }
  dimensions$label <- dimensions[[paste0("label_", language)]]
  dimensions
}

.nbs_validate_dimension_members <- function(dataset_id, filters) {
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  for (name in names(filters)) {
    members <- dimensions$value[dimensions$dimension == name]
    if (!length(members)) {
      .nbs_abort(
        "nbs_dataset_filter_error",
        sprintf(
          "Dataset `%s` has no dimension named `%s`.", dataset_id, name
        ),
        dataset_id = dataset_id,
        dimension = name
      )
    }
    if (!filters[[name]] %in% members) {
      .nbs_abort(
        "nbs_dataset_filter_error",
        sprintf(
          "Unknown `%s` member `%s` for dataset `%s`.",
          name, filters[[name]], dataset_id
        ),
        dataset_id = dataset_id,
        dimension = name,
        value = filters[[name]]
      )
    }
  }
  invisible(filters)
}

.nbs_exact_dataset_semantics <- function(dataset, filters) {
  period_policy <- dataset$period_policy[[1L]]
  aggregation_basis <- dataset$aggregation_basis[[1L]]
  seasonal_adjustment <- dataset$seasonal_adjustment[[1L]]
  period_basis <- filters$period_basis %||% ""
  measure <- filters$measure %||% ""
  frequency <- dataset$frequency[[1L]]

  if (period_basis == "ytd" ||
      dataset$dataset_id[[1L]] %in% c(
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY",
        "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION",
        "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION"
      )) {
    period_policy <- if (frequency == "M") {
      "ytd_january_not_reported_february_combined"
    } else {
      "all_periods"
    }
    aggregation_basis <- "year_to_date"
  }
  if (period_basis == "end_period") {
    period_policy <- "january_not_reported_february_first_period"
    aggregation_basis <- "single_period"
  }
  if (period_basis == "current") {
    period_policy <- if (
      dataset$dataset_id[[1L]] == "NBS/CN/M/RETAIL_SALES/DETAIL"
    ) {
      "current_january_february_not_separately_reported"
    } else {
      "all_periods"
    }
    aggregation_basis <- "single_period"
  }
  if (frequency == "A") {
    period_policy <- "all_periods"
    aggregation_basis <- "single_period"
  }
  if (frequency == "10D") {
    period_policy <- "all_periods"
    aggregation_basis <- "irregular_window"
    seasonal_adjustment <- "not_seasonally_adjusted"
  }
  if (grepl("official_sa", measure, fixed = TRUE)) {
    period_policy <- "all_periods"
    aggregation_basis <- "single_period"
    seasonal_adjustment <- "official_seasonally_adjusted"
  } else if (seasonal_adjustment == "mixed_by_measure") {
    seasonal_adjustment <- "not_seasonally_adjusted"
  }
  list(
    period_policy = period_policy,
    aggregation_basis = aggregation_basis,
    seasonal_adjustment = seasonal_adjustment
  )
}

.nbs_validate_tuple_compatibility <- function(dataset_id, filters) {
  valid <- nbs_dataset_valid_tuples(dataset_id)
  required <- setdiff(names(valid), "dimension_key")
  key <- nbs_dimension_key(filters[required])
  if (!key %in% valid$dimension_key) {
    .nbs_abort(
      "nbs_dataset_filter_error",
      sprintf(
        "The selected dimension tuple is not published for dataset `%s`.",
        dataset_id
      ),
      dataset_id = dataset_id,
      dimension_key = key
    )
  }
  invisible(filters)
}

#' Resolve one exact NBS dataset series descriptor
#'
#' Resolution is local and deterministic. Portal-backed leaves inherit exact
#' scalar semantics and units. Press-table measures use the unit and semantics
#' attached to their versioned dimension member. The descriptor is suitable
#' for a saved chart specification and can be used to validate a fetch before
#' any transport is attempted.
#'
#' @param dataset_id A stable ID from [nbs_dataset_catalogue()].
#' @param filters A named list selecting one exact leaf. Names must be the
#'   required dimensions from [nbs_dataset_dimensions()], or the single
#'   `series_id` escape hatch for an exact portal leaf.
#' @param language Display language, either `"en"` or `"zh"`.
#' @return A one-row data frame with these 31 columns, in this order:
#' `dataset_id`, `dimension_key`, `title`, `title_en`, `title_zh`, `unit`,
#' `unit_en`, `unit_zh`, `specification`, `specification_en`,
#' `specification_zh`, `product_unit`, `product_unit_en`, `product_unit_zh`,
#' `frequency`, `period_policy`, `aggregation_basis`,
#' `seasonal_adjustment`, `derived_method`, `source_vintage`, `route`,
#' `release_id`, `family`, `available_from`, `available_to`, `series_note`,
#' `series_note_en`, `series_note_zh`, `source_url`, `source_note`, and
#' `manifest_version`. All columns are character except the two
#' `available_*` columns, which are `Date`. `dimension_key` is the canonical
#' key produced by [nbs_dimension_key()]. The unsuffixed title, unit, and
#' notes follow `language`; English and Chinese values are always retained.
#' Empty specification and product-unit fields are meaningful for datasets
#' that do not publish those concepts.
#'
#' @details No network request, credential, or file write is involved.
#' `filters` must select a published tuple; member values and cross-dimension
#' compatibility are checked against bundled manifests. Invalid argument
#' shape raises `nbs_argument_error`; an unknown dataset raises
#' `nbs_unknown_dataset_error`; missing, unknown, or unpublished members raise
#' `nbs_dataset_filter_error`; malformed package data raises
#' `nbs_manifest_error`. Filter insertion order does not affect the canonical
#' descriptor or key.
#'
#' @seealso [nbs_dataset_dimensions()], [nbs_dataset_valid_tuples()],
#' [nbs_fetch_dataset()], [nbs_dimension_key()]
#'
#' @examples
#' descriptor <- nbs_resolve_dataset_series(
#'   "NBS/CN/M/CPI/PRICE_DETAIL",
#'   list(residence = "national", comparison = "yoy_index",
#'        category = "headline"),
#'   language = "en"
#' )
#' descriptor[, c("dimension_key", "title", "unit", "frequency")]
#' @export
nbs_resolve_dataset_series <- function(
    dataset_id,
    filters,
    language = c("en", "zh")) {
  dataset_id <- .nbs_scalar_character(dataset_id, "dataset_id")
  language <- match.arg(language)
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  if (!nrow(dataset)) {
    .nbs_abort(
      "nbs_unknown_dataset_error",
      sprintf("Unknown NBS dataset `%s`.", dataset_id),
      dataset_id = dataset_id
    )
  }
  filters <- .nbs_validate_dataset_filters(dataset, filters)
  if (!length(filters)) {
    .nbs_abort(
      "nbs_dataset_filter_error",
      "`filters` must select one exact dataset leaf."
    )
  }
  if (!"series_id" %in% names(filters)) {
    required <- strsplit(
      dataset$required_dimensions[[1L]], "|", fixed = TRUE
    )[[1L]]
    missing <- setdiff(required, names(filters))
    if (length(missing)) {
      .nbs_abort(
        "nbs_dataset_filter_error",
        sprintf(
          "Exact resolution for `%s` requires: %s.",
          dataset_id, paste(missing, collapse = ", ")
        ),
        missing_filters = missing
      )
    }
    # Exact descriptors are saved as chart metadata, so their presentation
    # must be independent of the order in which a caller built `filters`.
    filters <- filters[required]
  }
  .nbs_validate_dimension_members(dataset_id, filters)
  .nbs_validate_tuple_compatibility(dataset_id, filters)
  dimension_key <- nbs_dimension_key(filters)
  portal_series_id <- .nbs_dataset_portal_series_id(dataset_id, filters)
  if (nzchar(portal_series_id)) {
    descriptor <- .nbs_dataset_portal_descriptor(
      dataset, dimension_key, portal_series_id, filters, language
    )
    if (dataset_id == "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION") {
      coverage <- .nbs_dataset_snapshot_coverage(dataset_id, filters)
      descriptor$available_from <- min(
        descriptor$available_from, coverage$available_from, na.rm = TRUE
      )
      coverage_ends <- as.Date(c(
        descriptor$available_to[[1L]], coverage$available_to
      ))
      descriptor$available_to <- if (anyNA(coverage_ends)) {
        as.Date(NA)
      } else {
        max(coverage_ends)
      }
      descriptor$source_vintage <- dataset$source_vintage[[1L]]
      descriptor$route <- dataset$route[[1L]]
      descriptor$series_note <- dataset[[paste0(
        "series_note_", language
      )]][[1L]]
      descriptor$series_note_en <- dataset$series_note_en[[1L]]
      descriptor$series_note_zh <- dataset$series_note_zh[[1L]]
      descriptor$source_note <- dataset$source_note[[1L]]
    }
    return(descriptor)
  }

  if ("series_id" %in% names(filters)) {
    candidates <- .nbs_dataset_series_candidates(dataset_id)
    if (!filters$series_id %in% candidates) {
      leaf <- nbs_dataset_dimensions(dataset_id, language)
      leaf <- leaf[
        leaf$dimension == "series_id" &
          leaf$value == filters$series_id, , drop = FALSE
      ]
      if (nrow(leaf) != 1L) {
        .nbs_abort(
          "nbs_dataset_filter_error",
          sprintf(
            "Series `%s` is not a member of dataset `%s`.",
            filters$series_id, dataset_id
          )
        )
      }
      semantic <- leaf$semantics[[1L]]
      period_policy <- if (semantic == "year_to_date" &&
          dataset$frequency[[1L]] == "M") {
        "ytd_january_not_reported_february_combined"
      } else if (semantic == "end_period") {
        "january_not_reported_february_first_period"
      } else {
        "all_periods"
      }
      aggregation_basis <- if (semantic == "year_to_date") {
        "year_to_date"
      } else {
        "single_period"
      }
      title_en <- .nbs_dataset_display_title(
        dataset, leaf, filters, "en"
      )
      title_zh <- .nbs_dataset_display_title(
        dataset, leaf, filters, "zh"
      )
      return(data.frame(
        dataset_id = dataset_id,
        dimension_key = dimension_key,
        title = if (language == "en") title_en else title_zh,
        title_en = title_en,
        title_zh = title_zh,
        unit = if (language == "en") leaf$unit_en else leaf$unit_zh,
        unit_en = leaf$unit_en,
        unit_zh = leaf$unit_zh,
        specification = "",
        specification_en = "",
        specification_zh = "",
        product_unit = "",
        product_unit_en = "",
        product_unit_zh = "",
        frequency = dataset$frequency,
        period_policy = period_policy,
        aggregation_basis = aggregation_basis,
        seasonal_adjustment = dataset$seasonal_adjustment,
        derived_method = "none",
        source_vintage = dataset$source_vintage,
        route = dataset$route,
        release_id = dataset$release_id,
        family = dataset$family,
        available_from = as.Date(dataset$default_start),
        available_to = as.Date(ifelse(
          nzchar(dataset$available_to), dataset$available_to, NA_character_
        )),
        series_note = dataset[[paste0("series_note_", language)]],
        series_note_en = dataset$series_note_en,
        series_note_zh = dataset$series_note_zh,
        source_url = dataset$source_url,
        source_note = dataset$source_note,
        manifest_version = dataset$manifest_version,
        stringsAsFactors = FALSE
      ))
    }
  }

  dimensions <- nbs_dataset_dimensions(dataset_id, language)
  measure <- if ("measure" %in% names(filters)) {
    dimensions[
      dimensions$dimension == "measure" &
        dimensions$value == filters$measure,
      ,
      drop = FALSE
    ]
  } else {
    dimensions[FALSE, , drop = FALSE]
  }
  unit_en <- if (nrow(measure) && nzchar(measure$unit_en[[1L]])) {
    measure$unit_en[[1L]]
  } else {
    dataset$unit_en[[1L]]
  }
  unit_zh <- if (nrow(measure) && nzchar(measure$unit_zh[[1L]])) {
    measure$unit_zh[[1L]]
  } else {
    dataset$unit_zh[[1L]]
  }
  # Some exact archives (notably the Yearbook bridges) attach the published
  # unit to their leaf dimension rather than to a separate `measure` member.
  # Prefer one unambiguous leaf unit over a dataset-level placeholder.
  if (!nrow(measure) || !nzchar(measure$unit_en[[1L]])) {
    member_rows <- lapply(names(filters), function(name) {
      dimensions[
        dimensions$dimension == name &
          dimensions$value == filters[[name]], , drop = FALSE
      ]
    })
    member_rows <- Filter(function(rows) nrow(rows) == 1L, member_rows)
    member_units_en <- unique(Filter(nzchar, vapply(
      member_rows, function(rows) rows$unit_en[[1L]], character(1L)
    )))
    member_units_zh <- unique(Filter(nzchar, vapply(
      member_rows, function(rows) rows$unit_zh[[1L]], character(1L)
    )))
    if (length(member_units_en) == 1L) unit_en <- member_units_en[[1L]]
    if (length(member_units_zh) == 1L) unit_zh <- member_units_zh[[1L]]
  }
  product <- if ("product_code" %in% names(filters)) {
    dimensions[
      dimensions$dimension == "product_code" &
        dimensions$value == filters$product_code,
      ,
      drop = FALSE
    ]
  } else {
    dimensions[FALSE, , drop = FALSE]
  }
  specification_en <- if (nrow(product)) product$semantics[[1L]] else ""
  specification_zh <- specification_en
  product_unit_en <- if (nrow(product)) product$unit_en[[1L]] else ""
  product_unit_zh <- if (nrow(product)) product$unit_zh[[1L]] else ""
  if (dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES" &&
      filters$measure %in% c("price_yuan", "change_yuan") &&
      nrow(product) && nzchar(product_unit_en)) {
    unit_en <- paste("Yuan per", tolower(product_unit_en))
    unit_zh <- paste0("\u5143/", product_unit_zh)
  }
  seasonal_adjustment <- dataset$seasonal_adjustment[[1L]]
  if (nrow(measure) &&
      identical(measure$semantics[[1L]], "official_seasonally_adjusted")) {
    seasonal_adjustment <- "official_seasonally_adjusted"
  } else if (identical(seasonal_adjustment, "mixed_by_measure")) {
    seasonal_adjustment <- "not_seasonally_adjusted"
  }
  title_en <- .nbs_dataset_display_title(dataset, dimensions, filters, "en")
  title_zh <- .nbs_dataset_display_title(dataset, dimensions, filters, "zh")
  semantics <- .nbs_exact_dataset_semantics(dataset, filters)
  coverage <- .nbs_dataset_snapshot_coverage(dataset_id, filters)
  data.frame(
    dataset_id = dataset_id,
    dimension_key = dimension_key,
    title = if (language == "en") title_en else title_zh,
    title_en = title_en,
    title_zh = title_zh,
    unit = if (language == "en") unit_en else unit_zh,
    unit_en = unit_en,
    unit_zh = unit_zh,
    specification = if (language == "en") {
      specification_en
    } else {
      specification_zh
    },
    specification_en = specification_en,
    specification_zh = specification_zh,
    product_unit = if (language == "en") product_unit_en else product_unit_zh,
    product_unit_en = product_unit_en,
    product_unit_zh = product_unit_zh,
    frequency = dataset$frequency,
    period_policy = semantics$period_policy,
    aggregation_basis = semantics$aggregation_basis,
    seasonal_adjustment = semantics$seasonal_adjustment,
    derived_method = dataset$derived_method,
    source_vintage = dataset$source_vintage,
    route = dataset$route,
    release_id = dataset$release_id,
    family = dataset$family,
    available_from = coverage$available_from,
    available_to = coverage$available_to,
    series_note = dataset[[paste0("series_note_", language)]],
    series_note_en = dataset$series_note_en,
    series_note_zh = dataset$series_note_zh,
    source_url = dataset$source_url,
    source_note = dataset$source_note,
    manifest_version = dataset$manifest_version,
    stringsAsFactors = FALSE
  )
}

.nbs_validate_dataset_filters <- function(dataset, filters) {
  if (is.null(filters)) {
    filters <- list()
  }
  if (!is.list(filters) ||
      (length(filters) && (
        is.null(names(filters)) ||
        any(!nzchar(names(filters))) ||
        anyDuplicated(names(filters))
      ))) {
    .nbs_abort(
      "nbs_argument_error",
      "`filters` must be a named list with unique non-empty names."
    )
  }
  filters <- lapply(names(filters), function(name) {
    .nbs_scalar_character(as.character(filters[[name]]), paste0(
      "filters$", name
    ))
  }) |>
    stats::setNames(names(filters))

  # `series_id` is an exact-leaf escape hatch for portal-backed datasets. It
  # remains fail closed because membership is checked before any request.
  if ("series_id" %in% names(filters)) {
    if (!identical(names(filters), "series_id")) {
      .nbs_abort(
        "nbs_dataset_filter_error",
        "`series_id` cannot be combined with other dataset filters."
      )
    }
    return(filters)
  }

  required <- strsplit(
    dataset$required_dimensions[[1L]], "|", fixed = TRUE
  )[[1L]]
  unexpected <- setdiff(names(filters), required)
  if (length(unexpected)) {
    .nbs_abort(
      "nbs_dataset_filter_error",
      sprintf(
        "Dataset `%s` does not accept filters: %s.",
        dataset$dataset_id[[1L]], paste(unexpected, collapse = ", ")
      ),
      dataset_id = dataset$dataset_id[[1L]],
      unexpected_filters = unexpected
    )
  }
  missing <- setdiff(required, names(filters))
  if (length(missing) && !isTRUE(dataset$panel_allowed[[1L]])) {
    .nbs_abort(
      "nbs_dataset_filter_error",
      sprintf(
        "Dataset `%s` requires filters: %s.",
        dataset$dataset_id[[1L]], paste(missing, collapse = ", ")
      ),
      dataset_id = dataset$dataset_id[[1L]],
      missing_filters = missing
    )
  }
  filters
}

.nbs_dataset_series_candidates <- function(dataset_id) {
  catalogue <- nbs_catalogue("en")
  dimensions <- .nbs_load_dataset_dimensions()
  declared <- dimensions$value[
    dimensions$dataset_id == dataset_id &
      dimensions$dimension == "series_id"
  ]
  portal_map <- tryCatch(
    .nbs_load_dataset_portal_map(), error = function(error) NULL
  )
  mapped <- character()
  if (!is.null(portal_map)) {
    rows <- portal_map[portal_map$dataset_id == dataset_id, , drop = FALSE]
    mapped <- rows$series_id[rows$route_type == "exact"]
    stems <- rows$series_id[rows$route_type == "retail_category"]
    if (length(stems)) {
      stem_members <- vapply(catalogue$series_id, function(series_id) {
        any(startsWith(series_id, paste0(stems, "/")))
      }, logical(1))
      mapped <- c(mapped, catalogue$series_id[stem_members])
    }
  }
  intersect(catalogue$series_id, unique(c(declared, mapped)))
}

.nbs_dataset_portal_series_id <- function(dataset_id, filters) {
  if (identical(names(filters), "series_id")) {
    candidates <- .nbs_dataset_series_candidates(dataset_id)
    if (filters$series_id %in% candidates) {
      return(filters$series_id)
    }
    return("")
  }
  candidate <- .nbs_dataset_portal_map_series_id(dataset_id, filters)
  if (nzchar(candidate) && candidate %in% nbs_catalogue("en")$series_id) {
    candidate
  } else {
    ""
  }
}

.nbs_dataset_portal_descriptor <- function(
    dataset,
    dimension_key,
    series_id,
    filters,
    language) {
  scalar <- nbs_catalogue(language)
  scalar <- scalar[scalar$series_id == series_id, , drop = FALSE]
  if (nrow(scalar) != 1L) {
    .nbs_abort(
      "nbs_manifest_error",
      "A portal-mapped dataset leaf did not resolve to exactly one scalar."
    )
  }
  title_en <- scalar$title_en
  title_zh <- scalar$title_zh
  unit_en <- scalar$unit_en
  unit_zh <- scalar$unit_zh
  dimensions <- nbs_dataset_dimensions(dataset$dataset_id[[1L]], "en")
  title_en <- .nbs_dataset_display_title(dataset, dimensions, filters, "en")
  title_zh <- .nbs_dataset_display_title(dataset, dimensions, filters, "zh")
  if (!identical(names(filters), "series_id")) {
    if ("measure" %in% names(filters)) {
      measure <- dimensions[
        dimensions$dimension == "measure" &
          dimensions$value == filters$measure, , drop = FALSE
      ]
      if (nrow(measure) && nzchar(measure$unit_en[[1L]])) {
        unit_en <- measure$unit_en[[1L]]
      }
      if (nrow(measure) && nzchar(measure$unit_zh[[1L]])) {
        unit_zh <- measure$unit_zh[[1L]]
      }
    }
  }
  data.frame(
    dataset_id = dataset$dataset_id,
    dimension_key = dimension_key,
    title = if (language == "en") title_en else title_zh,
    title_en = title_en,
    title_zh = title_zh,
    unit = if (language == "en") unit_en else unit_zh,
    unit_en = unit_en,
    unit_zh = unit_zh,
    specification = "",
    specification_en = "",
    specification_zh = "",
    product_unit = "",
    product_unit_en = "",
    product_unit_zh = "",
    frequency = scalar$frequency,
    period_policy = scalar$period_policy,
    aggregation_basis = scalar$aggregation_basis,
    seasonal_adjustment = scalar$seasonal_adjustment,
    derived_method = scalar$derived_method,
    source_vintage = scalar$source_vintage,
    route = scalar$route,
    release_id = dataset$release_id,
    family = dataset$family,
    available_from = scalar$available_from,
    available_to = scalar$available_to,
    series_note = scalar$series_note,
    series_note_en = scalar$series_note_en,
    series_note_zh = scalar$series_note_zh,
    source_url = scalar$source_url,
    source_note = scalar$source_note,
    manifest_version = scalar$manifest_version,
    stringsAsFactors = FALSE
  )
}

.nbs_dataset_snapshot_coverage_cache <- new.env(parent = emptyenv())

.nbs_dataset_snapshot_coverage <- function(dataset_id, filters) {
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  fallback <- list(
    available_from = as.Date(dataset$default_start[[1L]]),
    available_to = as.Date(if (nzchar(dataset$available_to[[1L]])) {
      dataset$available_to[[1L]]
    } else {
      NA_character_
    })
  )
  dimension_key <- nbs_dimension_key(filters)
  cache_key <- paste(
    dataset$manifest_version[[1L]], dataset_id, dimension_key,
    sep = "\r"
  )
  if (exists(
      cache_key,
      envir = .nbs_dataset_snapshot_coverage_cache,
      inherits = FALSE
  )) {
    return(get(
      cache_key,
      envir = .nbs_dataset_snapshot_coverage_cache,
      inherits = FALSE
    ))
  }
  observations <- tryCatch(
    .nbs_press_observations_for_dataset(dataset_id),
    error = function(error) NULL
  )
  if (is.null(observations)) {
    return(fallback)
  }

  required <- strsplit(
    dataset$required_dimensions[[1L]], "|", fixed = TRUE
  )[[1L]]
  columns <- ifelse(required == "category_code", "category", required)
  dataset_observations <- observations[
    observations$dataset_id == dataset_id, , drop = FALSE
  ]
  result <- fallback
  if (nrow(dataset_observations) &&
      all(required %in% names(filters)) &&
      all(columns %in% names(dataset_observations))) {
    keep <- rep(TRUE, nrow(dataset_observations))
    for (index in seq_along(required)) {
      values <- as.character(dataset_observations[[columns[[index]]]])
      keep <- keep & .nbs_dataset_observation_matches(
        dataset_id,
        required[[index]],
        as.character(filters[[required[[index]]]]),
        values
      )
    }
    dataset_observations <- dataset_observations[keep, , drop = FALSE]
    releases <- .nbs_load_release_index()
    releases <- releases[releases$dataset_id == dataset_id, , drop = FALSE]
    release_match <- match(
      dataset_observations$release_period, releases$period
    )
    usable <- !is.na(release_match)
    if (any(usable)) {
      dataset_observations <- dataset_observations[usable, , drop = FALSE]
      release_match <- release_match[usable]
      row_start <- as.numeric(as.Date(
        releases$reference_period_start[release_match]
      ))
      row_end <- as.numeric(as.Date(
        releases$reference_period_end[release_match]
      ))
      observation_date <- as.numeric(dataset_observations$observation_date)
      has_observation_date <- !is.na(observation_date)
      row_start[has_observation_date] <- pmin(
        row_start[has_observation_date],
        observation_date[has_observation_date]
      )
      row_end[has_observation_date] <- pmax(
        row_end[has_observation_date],
        observation_date[has_observation_date]
      )
      result <- list(
        available_from = as.Date(min(row_start), origin = "1970-01-01"),
        available_to = as.Date(max(row_end), origin = "1970-01-01")
      )
    }
  }
  assign(
    cache_key,
    result,
    envir = .nbs_dataset_snapshot_coverage_cache
  )
  result
}

.nbs_dataset_bound <- function(value, frequency, bound) {
  if (frequency %in% c("M", "Q", "A")) {
    return(.nbs_period_date(value, frequency, bound))
  }
  if (inherits(value, "POSIXt")) {
    value <- as.Date(value)
  }
  if (!inherits(value, "Date")) {
    value <- suppressWarnings(as.Date(
      .nbs_scalar_character(as.character(value), "period bound")
    ))
  }
  if (length(value) != 1L || is.na(value)) {
    .nbs_abort(
      "nbs_period_error",
      "Ten-day dataset bounds must be one non-missing ISO date."
    )
  }
  value
}

.nbs_press_numeric_cells <- function(
    table,
    dataset_id,
    table_index,
    header_rows,
    keep_rows = NULL) {
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  if (nrow(table) <= header_rows || ncol(table) < 2L) {
    return(NULL)
  }
  data_indices <- seq.int(header_rows + 1L, nrow(table))
  if (!is.null(keep_rows)) {
    data_indices <- data_indices[keep_rows(table[data_indices, , drop = FALSE])]
  }
  headers <- vapply(seq_len(ncol(table)), function(column) {
    values <- trimws(as.character(table[seq_len(header_rows), column]))
    values <- unique(values[nzchar(values)])
    paste(values, collapse = " | ")
  }, character(1))
  rows <- list()
  cursor <- 0L
  for (row_index in data_indices) {
    row_label <- trimws(as.character(table[[1L]][[row_index]]))
    for (column_index in seq.int(2L, ncol(table))) {
      raw <- trimws(as.character(table[[column_index]][[row_index]]))
      normalized <- gsub(",", "", raw, fixed = TRUE)
      if (!grepl(
        "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", normalized
      )) {
        next
      }
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        dataset_id = dataset_id,
        table_index = table_index,
        row_index = row_index,
        column_index = column_index,
        row_label = row_label,
        column_label = headers[[column_index]],
        raw_value = raw,
        value = as.numeric(normalized),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) NULL else do.call(rbind, rows)
}

.nbs_parse_press_release <- function(html, dataset, release = NULL) {
  document <- tryCatch(
    xml2::read_html(html),
    error = function(error) error
  )
  if (inherits(document, "error")) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The NBS press release is not valid HTML.",
      parent = document
    )
  }
  tables <- rvest::html_table(document, fill = TRUE, header = FALSE)
  dataset_id <- dataset$dataset_id[[1L]]
  normalized <- .nbs_parse_press_dataset_specific(
    document, tables, dataset, release
  )
  if (!is.null(normalized)) {
    return(normalized)
  }
  not_notes <- function(x) {
    !grepl("^\\s*(Notes?|Source)\\s*:", x[[1L]], ignore.case = TRUE)
  }
  profile <- switch(
    dataset_id,
    "NBS/CN/M/INDUSTRIAL_ENTERPRISES/FINANCIALS_BY_CATEGORY" =
      list(indices = 1L, headers = 2L, keep = not_notes),
    "NBS/CN/M/INDUSTRIAL_ENTERPRISES/PERFORMANCE_BY_CATEGORY" =
      list(indices = 2L, headers = 3L, keep = not_notes),
    "NBS/CN/M/INDUSTRIAL_ENTERPRISES/FINANCIALS_BY_INDUSTRY" =
      list(indices = 3L, headers = 3L, keep = not_notes),
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" =
      list(indices = c(1L, 2L), headers = c(2L, 3L), keep = not_notes),
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY" =
      list(indices = 3L, headers = 3L, keep = not_notes),
    "NBS/CN/M/HOUSING_PRICES/70_CITY" =
      list(indices = seq_len(4L), headers = c(2L, 2L, 3L, 3L), keep = NULL),
    "NBS/CN/M/HOUSING_70_CITY" =
      list(indices = seq_len(4L), headers = c(2L, 2L, 3L, 3L), keep = NULL),
    "NBS/CN/M/PROPERTY/REGIONAL_RELEASE" =
      list(
        indices = c(2L, 3L), headers = c(2L, 3L),
        keep = function(x) !grepl("^National Total$", x[[1L]])
      ),
    "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION" =
      list(
        indices = c(2L, 3L), headers = c(2L, 3L),
        keep = function(x) !grepl("^National Total$", x[[1L]])
      ),
    "NBS/CN/M/RETAIL/DESIGNATED_GOODS_BY_CATEGORY" =
      list(
        indices = 1L, headers = 2L,
        keep = function(x) {
          labels <- trimws(x[[1L]])
          labels %in% labels[seq.int(
            max(1L, length(labels) - 15L), length(labels)
          )]
        }
      ),
    "NBS/CN/A/WAGES/RELEASE_TABLES" =
      list(
        indices = seq_along(tables),
        headers = vapply(tables, function(x) {
          if (ncol(x) > 4L) 3L else 2L
        }, integer(1)),
        keep = not_notes
      ),
    "NBS/CN/A/EMPLOYMENT_WAGES" =
      list(
        indices = seq_along(tables),
        headers = vapply(tables, function(x) {
          if (ncol(x) > 4L) 3L else 2L
        }, integer(1)),
        keep = not_notes
      ),
    "NBS/CN/10D/MEANS_OF_PRODUCTION" =
      list(
        indices = 1L, headers = 1L,
        keep = function(x) {
          grepl(
            "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$",
            gsub(",", "", trimws(x[[3L]]), fixed = TRUE)
          )
        }
      ),
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES" =
      list(
        indices = 1L, headers = 1L,
        keep = function(x) {
          grepl(
            "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$",
            gsub(",", "", trimws(x[[3L]]), fixed = TRUE)
          )
        }
      ),
    list(
      indices = seq_along(tables),
      headers = rep(1L, length(tables)),
      keep = not_notes
    )
  )
  if (!length(tables) || any(profile$indices > length(tables))) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf("NBS release `%s` has an unexpected table count.", dataset_id),
      dataset_id = dataset_id
    )
  }
  frames <- lapply(seq_along(profile$indices), function(index) {
    .nbs_press_numeric_cells(
      tables[[profile$indices[[index]]]],
      dataset_id,
      profile$indices[[index]],
      profile$headers[[index]],
      profile$keep
    )
  })
  frames <- Filter(Negate(is.null), frames)
  result <- if (length(frames)) do.call(rbind, frames) else data.frame()

  if (dataset_id %in% c(
    "NBS/CN/10D/MEANS_OF_PRODUCTION",
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
  )) {
    text <- rvest::html_text2(rvest::html_element(document, "body"))
    pattern <- paste0(
      "prices of ([0-9]+) products increased, ([0-9]+) kinds decreased, ",
      "and ([0-9]+) kinds remained flat"
    )
    hit <- regexec(pattern, text, ignore.case = TRUE)
    match <- regmatches(text, hit)[[1L]]
    if (length(match) == 4L) {
      direction <- data.frame(
        dataset_id = dataset_id,
        table_index = 0L,
        row_index = seq_len(3L),
        column_index = 1L,
        row_label = c("increased", "decreased", "remained_flat"),
        column_label = "product_count",
        raw_value = match[2:4],
        value = as.numeric(match[2:4]),
        stringsAsFactors = FALSE
      )
      result <- rbind(result, direction)
    }
  }
  rownames(result) <- NULL
  result
}

.nbs_dataset_result_metadata <- function(
    result,
    dataset,
    release,
    response,
    filters,
    language) {
  if (!nrow(result)) {
    return(result)
  }
  common_start <- as.Date(release$reference_period_start[[1L]])
  common_end <- as.Date(release$reference_period_end[[1L]])
  observation_date <- as.Date(result$observation_date)
  has_observation_date <- !is.na(observation_date)
  reference_start <- rep(common_start, nrow(result))
  reference_end <- rep(common_end, nrow(result))
  reference_start[has_observation_date] <- observation_date[has_observation_date]
  if (any(has_observation_date)) {
    observation_end <- switch(
      dataset$frequency[[1L]],
      M = .nbs_next_period(observation_date[has_observation_date], "M") - 1,
      Q = .nbs_next_period(observation_date[has_observation_date], "Q") - 1,
      A = as.Date(sprintf(
        "%s-12-31", format(observation_date[has_observation_date], "%Y")
      )),
      `10D` = observation_date[has_observation_date]
    )
    reference_end[has_observation_date] <- observation_end
  }
  is_current <- result$period_basis %in% c("current", "end_period")
  reference_start[is_current & !has_observation_date] <-
    as.Date(sprintf(
      "%s-%s-01",
      format(common_end, "%Y"), format(common_end, "%m")
    ))
  reported_date <- switch(
    dataset$frequency[[1L]],
    M = as.Date(format(common_end, "%Y-%m-01")),
    Q = as.Date(sprintf(
      "%s-%02d-01",
      format(common_end, "%Y"),
      ((as.integer(format(common_end, "%m")) - 1L) %/% 3L) * 3L + 1L
    )),
    A = as.Date(sprintf("%s-01-01", format(common_end, "%Y"))),
    `10D` = common_start
  )
  date <- rep(reported_date, nrow(result))
  date[has_observation_date] <- observation_date[has_observation_date]
  period <- rep(release$period[[1L]], nrow(result))
  if (any(has_observation_date)) {
    observation_period <- switch(
      dataset$frequency[[1L]],
      M = format(observation_date[has_observation_date], "%Y-%m"),
      Q = sprintf(
        "%s-Q%d",
        format(observation_date[has_observation_date], "%Y"),
        (as.integer(format(observation_date[has_observation_date], "%m")) - 1L) %/%
          3L + 1L
      ),
      A = format(observation_date[has_observation_date], "%Y"),
      `10D` = format(observation_date[has_observation_date])
    )
    period[has_observation_date] <- observation_period
  }
  aggregation_basis <- rep(
    dataset$aggregation_basis[[1L]], nrow(result)
  )
  aggregation_basis[result$period_basis == "ytd"] <- "year_to_date"
  aggregation_basis[result$period_basis %in% c(
    "current", "end_period", "annual"
  )] <- "single_period"
  aggregation_basis[result$period_basis == "ten_day"] <- "irregular_window"
  seasonal_adjustment <- rep(
    dataset$seasonal_adjustment[[1L]], nrow(result)
  )
  seasonal_adjustment[seasonal_adjustment == "mixed_by_measure"] <-
    "not_seasonally_adjusted"
  seasonal_adjustment[
    grepl("official_sa", result$measure, fixed = TRUE)
  ] <- "official_seasonally_adjusted"
  period_policy <- vapply(seq_len(nrow(result)), function(index) {
    semantics <- .nbs_exact_dataset_semantics(
      dataset,
      list(
        period_basis = result$period_basis[[index]],
        measure = result$measure[[index]]
      )
    )
    semantics$period_policy
  }, character(1))
  dimension_columns <- strsplit(
    dataset$required_dimensions[[1L]], "|", fixed = TRUE
  )[[1L]]
  dimension_columns <- intersect(
    sub("^category_code$", "category", dimension_columns), names(result)
  )
  row_keys <- vapply(seq_len(nrow(result)), function(index) {
    values <- as.list(result[index, dimension_columns, drop = FALSE])
    names(values)[names(values) == "category"] <- if (
      "category_code" %in% strsplit(
        dataset$required_dimensions[[1L]], "|", fixed = TRUE
      )[[1L]]
    ) "category_code" else "category"
    nbs_dimension_key(values)
  }, character(1))
  requested_key <- nbs_dimension_key(filters)
  if (length(filters)) {
    row_keys[] <- requested_key
  }
  display_dimensions <- nbs_dataset_dimensions(
    dataset$dataset_id[[1L]], "en"
  )
  member_filters <- lapply(seq_len(nrow(result)), function(index) {
    values <- as.list(result[index, dimension_columns, drop = FALSE])
    names(values)[names(values) == "category"] <- if (
      "category_code" %in% strsplit(
        dataset$required_dimensions[[1L]], "|", fixed = TRUE
      )[[1L]]
    ) "category_code" else "category"
    values
  })
  result$dimension_label_en <- vapply(member_filters, function(values) {
    paste(
      .nbs_dataset_member_labels(display_dimensions, values, "en"),
      collapse = " \u2014 "
    )
  }, character(1L))
  result$dimension_label_zh <- vapply(member_filters, function(values) {
    paste(
      .nbs_dataset_member_labels(display_dimensions, values, "zh"),
      collapse = "\u2014"
    )
  }, character(1L))
  result$release_id <- dataset$release_id[[1L]]
  result$family <- dataset$family[[1L]]
  result$dimension_key <- row_keys
  result$dimension_label <- if (
    language == "en" && "dimension_label_en" %in% names(result)
  ) {
    result$dimension_label_en
  } else if (
    language == "zh" && "dimension_label_zh" %in% names(result)
  ) {
    result$dimension_label_zh
  } else {
    result$row_label
  }
  if (!"route" %in% names(result)) {
    result$route <- rep(dataset$route[[1L]], nrow(result))
  } else {
    missing_route <- is.na(result$route) | !nzchar(trimws(result$route))
    result$route[missing_route] <- dataset$route[[1L]]
  }
  result$frequency <- dataset$frequency[[1L]]
  result$date <- date
  result$period <- period
  result$reference_date <- date
  result$reference_period <- period
  result$reference_period_start <- reference_start
  result$reference_period_end <- reference_end
  result$span_months <- pmax(
    1L,
    (as.integer(format(reference_end, "%Y")) -
      as.integer(format(reference_start, "%Y"))) * 12L +
      as.integer(format(reference_end, "%m")) -
      as.integer(format(reference_start, "%m")) + 1L
  )
  result$span_months[aggregation_basis == "irregular_window"] <- NA_integer_
  result$aggregation_basis <- aggregation_basis
  result$period_policy <- period_policy
  result$release_date <- as.Date(release$release_date[[1L]])
  result$vintage_date <- as.Date(release$release_date[[1L]])
  result$data_status <- "first_release_snapshot"
  result$seasonal_adjustment <- seasonal_adjustment
  result$derived_method <- "none"
  if (!"source_vintage" %in% names(result)) {
    result$source_vintage <- rep(dataset$source_vintage[[1L]], nrow(result))
  } else {
    missing_vintage <- is.na(result$source_vintage) |
      !nzchar(trimws(result$source_vintage))
    result$source_vintage[missing_vintage] <- dataset$source_vintage[[1L]]
  }
  result$source_observation_periods <- period
  result$is_missing <- is.na(result$value)
  result$source_url <- release$source_url[[1L]]
  result$series_note <- dataset[[
    paste0("series_note_", language)
  ]][[1L]]
  result$series_note_en <- dataset$series_note_en[[1L]]
  result$series_note_zh <- dataset$series_note_zh[[1L]]
  localized_unit <- paste0("unit_", language)
  canonical_unit <- if ("unit" %in% names(result)) {
    as.character(result$unit)
  } else {
    rep(NA_character_, nrow(result))
  }
  missing_unit <- is.na(canonical_unit) | !nzchar(trimws(canonical_unit))
  if (any(missing_unit) && localized_unit %in% names(result)) {
    localized <- as.character(result[[localized_unit]])
    usable <- missing_unit & !is.na(localized) & nzchar(trimws(localized))
    canonical_unit[usable] <- localized[usable]
    missing_unit <- is.na(canonical_unit) | !nzchar(trimws(canonical_unit))
  }
  if (length(filters)) {
    descriptor <- nbs_resolve_dataset_series(
      dataset$dataset_id[[1L]], filters, language
    )
    descriptor_unit <- descriptor$unit[[1L]]
    equivalent <- !missing_unit &
      tolower(trimws(canonical_unit)) == tolower(trimws(descriptor_unit))
    conflict <- !missing_unit & !equivalent
    if (any(conflict)) {
      .nbs_abort(
        "nbs_dataset_contract_error",
        "Dataset observations conflict with exact canonical unit metadata.",
        dataset_id = dataset$dataset_id[[1L]],
        observed_units = unique(canonical_unit[conflict]),
        expected_unit = descriptor_unit
      )
    }
    canonical_unit[missing_unit | equivalent] <- descriptor_unit
    missing_unit <- is.na(canonical_unit) | !nzchar(trimws(canonical_unit))
  }
  if (any(missing_unit)) {
    .nbs_abort(
      "nbs_dataset_contract_error",
      "Dataset observations have no canonical unit metadata.",
      dataset_id = dataset$dataset_id[[1L]]
    )
  }
  result$unit <- canonical_unit
  result$source_note <- dataset$source_note[[1L]]
  result$retrieved_at <- response$retrieved_at
  result$raw_sha256 <- response$raw_sha256
  result$source_raw_sha256 <- response$raw_sha256
  result$snapshot_archive_sha256 <- release$snapshot_sha256[[1L]]
  result$manifest_version <- dataset$manifest_version[[1L]]
  result$parser_version <- "4"
  result$package_version <- .nbs_package_version()
  result
}

.nbs_dataset_observation_provenance_fields <- c(
  "dataset_id", "dimension_key", "release_id", "family", "frequency",
  "period_policy", "unit", "source_note", "seasonal_adjustment",
  "derived_method", "source_vintage", "manifest_version",
  "package_version", "source_url", "route", "aggregation_basis",
  "series_note"
)

.nbs_finalize_dataset_result <- function(result, dataset_id) {
  missing <- setdiff(
    .nbs_dataset_observation_provenance_fields,
    names(result)
  )
  if (length(missing)) {
    .nbs_abort(
      "nbs_dataset_contract_error",
      sprintf(
        "Dataset observations are missing canonical provenance: %s.",
        paste(missing, collapse = ", ")
      ),
      dataset_id = dataset_id,
      missing_fields = missing
    )
  }
  invalid <- vapply(
    .nbs_dataset_observation_provenance_fields,
    function(name) {
      value <- as.character(result[[name]])
      length(value) != nrow(result) || anyNA(value) ||
        any(!nzchar(trimws(value)))
    },
    logical(1L)
  )
  if (any(invalid)) {
    .nbs_abort(
      "nbs_dataset_contract_error",
      sprintf(
        "Dataset observations have empty canonical provenance: %s.",
        paste(names(invalid)[invalid], collapse = ", ")
      ),
      dataset_id = dataset_id,
      invalid_fields = names(invalid)[invalid]
    )
  }
  generic_unit <- grepl(
    "official (published|product) unit|varies|various",
    result$unit,
    ignore.case = TRUE
  )
  if (any(generic_unit)) {
    .nbs_abort(
      "nbs_dataset_contract_error",
      "Dataset observations have generic canonical unit metadata.",
      dataset_id = dataset_id
    )
  }
  result
}

.nbs_capacity_result_metadata <- function(result, dataset, language) {
  first_release <- !is.na(result$data_status) &
    result$data_status == "first_release_snapshot"
  result$route[first_release] <- "press_snapshot"
  result$source_vintage[first_release] <- "first_release_press_archive"
  result$series_note <- dataset[[paste0("series_note_", language)]][[1L]]
  result$series_note_en <- dataset$series_note_en[[1L]]
  result$series_note_zh <- dataset$series_note_zh[[1L]]
  result$source_note <- dataset$source_note[[1L]]
  result
}

#' Fetch an NBS dimensional dataset
#'
#' Portal-backed datasets accept the exact `series_id` of a member leaf as a
#' filter and delegate to [nbs_fetch_series()]. Press-table datasets fetch the
#' bundled normalized first-release observations and reject unexpected release
#' cardinality. Historical pages are crawled only by the opt-in maintenance
#' generator, never by this runtime function.
#'
#' @param dataset_id A stable ID from [nbs_dataset_catalogue()].
#' @param filters A named list of exact dimension filters. For a portal-backed
#'   dataset, `list(series_id = "...")` selects one verified UUID leaf.
#' @param start,end Optional period bounds. These apply to portal series and to
#'   the reference windows of bundled press releases.
#' @param language Portal and display language, either `"en"` or `"zh"`.
#' @param client An object returned by [nbs_client()].
#' @param use_cache Whether portal leaf requests may use the client cache.
#' @return A long-form base data frame, ordered oldest first by canonical
#' `date`, carrying one row per published observation or table cell. Every
#' result contains the following canonical columns: `dataset_id`,
#' `dimension_key`, `release_id`, `family`, `frequency`, `period_policy`,
#' `unit`, `source_note`, `seasonal_adjustment`, `derived_method`,
#' `source_vintage`, `manifest_version`, `package_version`, `source_url`,
#' `route`, `aggregation_basis`, and `series_note`. It also includes the
#' observation contract columns `date` (`Date`), `period` (character),
#' `reference_date` (`Date`), `reference_period` (character),
#' `reference_period_start` and `reference_period_end` (`Date`),
#' `span_months` (integer except `NA` for irregular ten-day windows),
#' `release_date` and `vintage_date` (`Date`), `data_status` (character),
#' `source_observation_periods` (character), `is_missing` (logical),
#' `retrieved_at` (`POSIXct`), `raw_sha256`, `source_raw_sha256`, and
#' `parser_version` (character). Bundled archive routes additionally retain
#' `snapshot_archive_sha256` when that archive hash is available. Bilingual
#' `series_note_en`/`series_note_zh`, `dimension_label_en`/
#' `dimension_label_zh`, and `unit_en`/`unit_zh` are retained. `value` is
#' numeric and `raw_value` is the source text where the route supplies it.
#' Route-specific source columns (for example `measure`, `category`,
#' `product_code`, `table_index`, or portal UUID fields) are preserved
#' alongside the canonical metadata; their exact set and position depend on
#' the dataset's published schema. Missing numeric observations are represented by
#' `value = NA` and `is_missing = TRUE`; the canonical provenance fields are
#' non-empty.
#'
#' @details Empty `filters` returns a complete panel when the dataset declares
#' `panel_allowed = TRUE`. Otherwise every required dimension must be
#' supplied, and it must be a published tuple from
#' [nbs_dataset_valid_tuples()]. Portal leaves may make HTTPS requests through
#' `client`; bundled press, yearbook, and retained longitudinal routes are
#' offline. Only portal observations use the per-client in-memory cache, and
#' `use_cache = FALSE` bypasses it; no package function writes credentials,
#' home-directory files, or a global cache. Requests, parsing, cardinality,
#' range, and provenance checks fail closed. Conditions include
#' `nbs_argument_error`, `nbs_unknown_dataset_error`,
#' `nbs_dataset_filter_error`, `nbs_range_error`, `nbs_manifest_error`,
#' `nbs_period_error`, `nbs_press_schema_error`,
#' `nbs_dataset_cardinality_error`, and `nbs_dataset_contract_error`.
#' Portal-backed paths can additionally signal `nbs_transport_error`,
#' `nbs_transport_contract_error`, `nbs_http_error`, `nbs_endpoint_error`,
#' `nbs_waf_error`, `nbs_json_error`, `nbs_api_error`, or `nbs_schema_error`.
#' Because an exact portal leaf delegates to [nbs_fetch_series()], all of that
#' function's series, period-limit, manifest-gap, period-schema, overlap, and
#' incomplete-period conditions also apply. The bundled path does not require
#' credentials or network access.
#'
#' @seealso [nbs_dataset_catalogue()], [nbs_dataset_valid_tuples()],
#' [nbs_resolve_dataset_series()], [nbs_fetch_series()], [nbs_client()]
#'
#' @examples
#' cpi_file <- system.file(
#'   "extdata", "examples", "cpi-2026-june.json", package = "readNBS"
#' )
#' cpi_body <- readBin(cpi_file, "raw", n = file.info(cpi_file)$size)
#' fixture_client <- nbs_client(
#'   transport = function(request) {
#'     stopifnot(identical(request$path, "stream/esData"))
#'     list(
#'       status = 200L,
#'       headers = list(`content-type` = "application/json"),
#'       body = cpi_body
#'     )
#'   },
#'   retries = 0L,
#'   now = function() as.POSIXct("2026-07-29 12:00:00", tz = "UTC")
#' )
#' cpi <- nbs_fetch_dataset(
#'   "NBS/CN/M/CPI/PRICE_DETAIL",
#'   filters = list(
#'     residence = "national", comparison = "yoy_index", category = "headline"
#'   ),
#'   start = "2026-06", end = "2026-06", client = fixture_client
#' )
#' cpi[, c("date", "period", "value", "unit", "dimension_key")]
#' @export
nbs_fetch_dataset <- function(
    dataset_id,
    filters = list(),
    start = NULL,
    end = NULL,
    language = c("en", "zh"),
    client = nbs_client(),
    use_cache = TRUE) {
  dataset_id <- .nbs_scalar_character(dataset_id, "dataset_id")
  language <- match.arg(language)
  .nbs_validate_client(client)
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  if (!nrow(dataset)) {
    .nbs_abort(
      "nbs_unknown_dataset_error",
      sprintf("Unknown NBS dataset `%s`.", dataset_id),
      dataset_id = dataset_id
    )
  }
  filters <- .nbs_validate_dataset_filters(dataset, filters)
  if (length(filters)) {
    .nbs_validate_dimension_members(dataset_id, filters)
  }
  required <- strsplit(
    dataset$required_dimensions[[1L]], "|", fixed = TRUE
  )[[1L]]
  exact_filters <- identical(names(filters), "series_id") ||
    all(required %in% names(filters))
  portal_result <- NULL
  portal_series_id <- if (exact_filters &&
      !identical(dataset$route[[1L]], "yearbook_snapshot") &&
      !isTRUE(client$press_fixture_mode %||% FALSE)) {
    .nbs_dataset_portal_series_id(dataset_id, filters)
  } else {
    ""
  }
  if (nzchar(portal_series_id)) {
    portal_start <- start
    portal_end <- end
    # A panel advertises a family-wide default start, while a particular
    # official leaf can begin later.  Clamp before transport rather than
    # asking the scalar layer to manufacture coverage before that leaf exists.
    # This applies to every exact portal panel (not just capacity utilisation).
    scalar <- nbs_catalogue(language)
    scalar <- scalar[scalar$series_id == portal_series_id, , drop = FALSE]
    if (nrow(scalar) != 1L) {
      .nbs_abort(
        "nbs_manifest_error",
        "A portal panel leaf did not resolve uniquely in the scalar catalogue."
      )
    }
    scalar_start <- as.Date(scalar$available_from[[1L]])
    scalar_end <- as.Date(scalar$available_to[[1L]])
    requested_start <- if (is.null(start)) {
      scalar_start
    } else {
      .nbs_dataset_bound(start, dataset$frequency[[1L]], "start")
    }
    requested_end <- if (is.null(end)) {
      scalar_end
    } else {
      .nbs_dataset_bound(end, dataset$frequency[[1L]], "end")
    }
    request_portal <- (is.na(requested_end) || requested_end >= scalar_start) &&
      (is.na(scalar_end) || requested_start <= scalar_end)
    if (request_portal) {
      portal_start <- max(requested_start, scalar_start, na.rm = TRUE)
      if (!is.na(scalar_end)) {
        portal_end <- min(requested_end, scalar_end, na.rm = TRUE)
      }
    }
    if (request_portal) {
    portal_result <- nbs_fetch_series(
      portal_series_id,
      start = portal_start,
      end = portal_end,
      language = language,
      client = client,
      use_cache = use_cache
    )
    portal_result$dataset_id <- dataset_id
    portal_result$release_id <- dataset$release_id[[1L]]
    portal_result$family <- dataset$family[[1L]]
    portal_result$dimension_key <- nbs_dimension_key(filters)
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters, language)
    portal_result$title <- descriptor$title[[1L]]
    portal_result$title_en <- descriptor$title_en[[1L]]
    portal_result$title_zh <- descriptor$title_zh[[1L]]
    portal_result$dimension_label <- descriptor$title[[1L]]
    portal_result$dimension_label_en <- descriptor$title_en[[1L]]
    portal_result$dimension_label_zh <- descriptor$title_zh[[1L]]
    if (dataset_id != "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION") {
      return(.nbs_finalize_dataset_result(portal_result, dataset_id))
    }
    portal_result <- .nbs_capacity_result_metadata(
      portal_result, dataset, language
    )
    }
  }

  if (length(filters)) {
    missing <- setdiff(required, names(filters))
    if (length(missing)) {
      .nbs_abort(
        "nbs_dataset_filter_error",
        sprintf(
          "Exact dataset fetch for `%s` requires: %s.",
          dataset_id, paste(missing, collapse = ", ")
        ),
        missing_filters = missing
      )
    }
    .nbs_validate_tuple_compatibility(dataset_id, filters)
  }

  releases <- .nbs_load_release_index()
  yearbook_releases <- .nbs_load_yearbook_release_index()
  yearbook_releases$expected_press_cells <-
    yearbook_releases$expected_yearbook_cells
  for (column in setdiff(names(releases), names(yearbook_releases))) {
    yearbook_releases[[column]] <- ""
  }
  releases <- rbind(releases, yearbook_releases[names(releases)])
  releases <- releases[releases$dataset_id == dataset_id, , drop = FALSE]
  if (isTRUE(client$press_fixture_mode %||% FALSE) && nrow(releases) &&
      !identical(dataset$route[[1L]], "yearbook_snapshot")) {
    fixture_release <- releases[
      releases$source_url == dataset$source_url[[1L]], , drop = FALSE
    ]
    if (!nrow(fixture_release)) {
      fixture_release <- releases[
        which.max(as.Date(releases$reference_period_end)), , drop = FALSE
      ]
    }
    releases <- fixture_release[1L, , drop = FALSE]
    releases$source_url <- dataset$source_url[[1L]]
    if (dataset_id == "NBS/CN/M/RETAIL_SALES/DETAIL") {
      releases$expected_press_cells <- 115L
    }
    releases$storage <- "live_document"
  }
  if (!nrow(releases)) {
    if (!is.null(portal_result)) {
      return(.nbs_finalize_dataset_result(portal_result, dataset_id))
    }
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("No release index is bundled for `%s`.", dataset_id)
    )
  }
  start_date <- if (is.null(start)) {
    min(as.Date(releases$reference_period_start))
  } else {
    .nbs_dataset_bound(start, dataset$frequency[[1L]], "start")
  }
  end_date <- if (is.null(end)) {
    max(as.Date(releases$reference_period_end))
  } else {
    .nbs_dataset_bound(end, dataset$frequency[[1L]], "end")
  }
  in_range <- if (dataset$frequency[[1L]] == "10D") {
    as.Date(releases$reference_period_end) >= start_date &
      as.Date(releases$reference_period_start) <= end_date
  } else {
    reported_date <- .nbs_floor_date(
      as.Date(releases$reference_period_end), dataset$frequency[[1L]]
    )
    reported_date >= start_date & reported_date <= end_date
  }
  if (dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES") {
    reported_date <- .nbs_floor_date(
      as.Date(releases$reference_period_end), "A"
    )
    prior_observation_date <- as.Date(sprintf(
      "%d-01-01", as.integer(format(reported_date, "%Y")) - 1L
    ))
    in_range <- in_range |
      (prior_observation_date >= start_date & prior_observation_date <= end_date)
  }
  embedded_observation_datasets <- c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024",
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
  )
  if (dataset_id %in% embedded_observation_datasets) {
    # These official releases contain explicitly dated observations spanning
    # more than their publication/reference year. Select the retained release
    # first, then apply the caller's bounds to its observation dates below.
    in_range[] <- TRUE
  }
  releases <- releases[
    in_range,
    ,
    drop = FALSE
  ]
  if (!nrow(releases)) {
    if (!is.null(portal_result)) {
      return(.nbs_finalize_dataset_result(portal_result, dataset_id))
    }
    .nbs_abort(
      "nbs_range_error",
      sprintf("No bundled `%s` release intersects the requested range.", dataset_id)
    )
  }
  frames <- lapply(seq_len(nrow(releases)), function(index) {
    release <- releases[index, , drop = FALSE]
    if (identical(release$storage[[1L]], "snapshot")) {
      archive <- .nbs_press_observations_for_release(
        dataset_id, release$period[[1L]]
      )
      raw_result <- archive[
        archive$dataset_id == dataset_id &
          archive$release_period == release$period[[1L]],
        ,
        drop = FALSE
      ]
      response <- list(
        retrieved_at = as.POSIXct(NA),
        raw_sha256 = release$source_capture_sha256[[1L]]
      )
    } else if (identical(release$storage[[1L]], "yearbook_snapshot")) {
      archive <- .nbs_load_yearbook_observations()
      raw_result <- archive[
        archive$dataset_id == dataset_id &
          archive$release_period == release$period[[1L]],
        ,
        drop = FALSE
      ]
      response <- list(
        retrieved_at = as.POSIXct(NA),
        raw_sha256 = release$source_capture_sha256[[1L]]
      )
    } else if (identical(release$storage[[1L]], "live_document")) {
      response <- .nbs_request_document(
        client,
        release$source_url[[1L]],
        operation = paste("NBS press release", dataset_id)
      )
      raw_result <- .nbs_parse_press_release(
        response$body_text, dataset, release
      )
    } else {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf(
          "Release `%s` has unsupported storage `%s`.",
          release$period[[1L]], release$storage[[1L]]
        )
      )
    }
    expected <- release$expected_press_cells[[1L]]
    if (nrow(raw_result) != expected) {
      .nbs_abort(
        "nbs_dataset_cardinality_error",
        sprintf(
          "Dataset `%s` returned %d numeric cells; expected exactly %d.",
          dataset_id, nrow(raw_result), expected
        ),
        dataset_id = dataset_id,
        expected_cells = expected,
        actual_cells = nrow(raw_result),
        raw_sha256 = response$raw_sha256
      )
    }
    result <- raw_result
    for (name in names(filters)) {
      column <- if (name == "category_code") "category" else name
      result <- result[
        .nbs_dataset_observation_matches(
          dataset_id, name, filters[[name]], result[[column]]
        ),
        ,
        drop = FALSE
      ]
    }
    if (length(filters) && !nrow(result)) {
      return(NULL)
    }
    .nbs_dataset_result_metadata(
      result, dataset, release, response, filters, language
    )
  })
  frames <- Filter(Negate(is.null), frames)
  if (!length(frames)) {
    if (!is.null(portal_result)) {
      return(.nbs_finalize_dataset_result(portal_result, dataset_id))
    }
    .nbs_abort(
      "nbs_range_error",
      sprintf(
        "The exact tuple has no published observations in the requested range."
      ),
      dataset_id = dataset_id,
      dimension_key = nbs_dimension_key(filters)
    )
  }
  # Independently frozen release supplements can carry a narrower provenance
  # column set than the current all-release archive.  Preserve every field
  # while making a mixed historical request composable.
  frame_columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(frame) {
    for (column in setdiff(frame_columns, names(frame))) frame[[column]] <- NA
    frame[frame_columns]
  })
  result <- do.call(rbind, frames)
  observation_bounded_datasets <- c(
    "NBS/CN/A/EMPLOYMENT_WAGES", embedded_observation_datasets
  )
  if (dataset_id %in% observation_bounded_datasets) {
    result <- result[
      as.Date(result$date) >= start_date & as.Date(result$date) <= end_date,
      ,
      drop = FALSE
    ]
    if (!nrow(result)) {
      .nbs_abort(
        "nbs_range_error",
        "The exact tuple has no published observation in the requested range.",
        dataset_id = dataset_id,
        dimension_key = nbs_dimension_key(filters)
      )
    }
  }
  if (!is.null(portal_result)) {
    portal_start <- min(as.Date(portal_result$date))
    result <- result[as.Date(result$date) < portal_start, , drop = FALSE]
    columns <- union(names(result), names(portal_result))
    for (column in setdiff(columns, names(result))) result[[column]] <- NA
    for (column in setdiff(columns, names(portal_result))) {
      portal_result[[column]] <- NA
    }
    result <- rbind(result[columns], portal_result[columns])
    result <- result[order(as.Date(result$date)), , drop = FALSE]
  }
  if (dataset_id == "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION") {
    result <- .nbs_capacity_result_metadata(result, dataset, language)
  }
  if (dataset_id %in% c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  )) {
    result$data_status <- "latest_retained_official_observation"
    result$route <- "retained_longitudinal_series"
    result$source_vintage <- "latest_retained_official_observation"
  }
  result <- result[order(as.Date(result$date), method = "radix"), , drop = FALSE]
  if (length(filters)) {
    duplicate_window <- duplicated(paste(
      result$reference_period_start, result$reference_period_end
    ))
    if (any(duplicate_window)) {
      .nbs_abort(
        "nbs_dataset_cardinality_error",
        "An exact dataset tuple returned more than one value per reference window.",
        dataset_id = dataset_id,
        dimension_key = nbs_dimension_key(filters)
      )
    }
  }
  rownames(result) <- NULL
  .nbs_finalize_dataset_result(result, dataset_id)
}
