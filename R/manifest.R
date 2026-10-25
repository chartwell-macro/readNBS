.nbs_manifest_columns <- c(
  "manifest_version", "series_id", "title_en", "title_zh",
  "description_en", "description_zh", "frequency", "root_uuid",
  "catalog_uuid", "indicator_uuid", "slice_start", "slice_end",
  "default_start", "classification_id", "stitch_policy", "period_policy",
  "aggregation_basis", "seasonal_adjustment",
  "series_note_en",
  "series_note_zh", "unit_en", "unit_zh", "scale", "geography_code",
  "geography_en", "geography_zh", "source_url", "source_note"
)

.nbs_manifest_cache <- new.env(parent = emptyenv())
.nbs_catalogue_cache <- new.env(parent = emptyenv())

.nbs_load_manifest <- function() {
  if (exists("manifest", envir = .nbs_manifest_cache, inherits = FALSE)) {
    return(get("manifest", envir = .nbs_manifest_cache, inherits = FALSE))
  }
  path <- system.file(
    "extdata", "nbs-series-manifest.csv",
    package = "readNBS",
    mustWork = FALSE
  )
  if (!nzchar(path)) {
    candidate <- file.path("inst", "extdata", "nbs-series-manifest.csv")
    if (file.exists(candidate)) {
      path <- candidate
    }
  }
  if (!nzchar(path) || !file.exists(path)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS series manifest could not be found."
    )
  }

  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  # Large exact portal panels live in focused supplemental manifests.  This
  # keeps the hand-curated scalar catalogue reviewable while retaining the
  # same validation and transport contract for every exposed UUID.
  supplements <- c(
    "nbs-price-series-manifest.csv",
    "nbs-key-output-series-manifest.csv",
    "nbs-industrial-series-manifest.csv.gz",
    "nbs-industrial-longitudinal-series-manifest.csv.gz"
  )
  for (filename in supplements) {
    supplement <- .nbs_extdata_path(filename)
    manifest <- rbind(manifest, utils::read.csv(
      supplement,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    ))
  }
  manifest$scale <- suppressWarnings(as.numeric(manifest$scale))
  manifest <- .nbs_validate_manifest(manifest)
  assign("manifest", manifest, envir = .nbs_manifest_cache)
  manifest
}

.nbs_validate_manifest <- function(manifest) {
  if (!is.data.frame(manifest)) {
    .nbs_abort("nbs_manifest_error", "The NBS manifest must be a data frame.")
  }
  missing_columns <- setdiff(.nbs_manifest_columns, names(manifest))
  if (length(missing_columns)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "The NBS manifest is missing required columns: %s.",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  if (!nrow(manifest)) {
    .nbs_abort("nbs_manifest_error", "The NBS manifest is empty.")
  }

  required_text <- setdiff(
    .nbs_manifest_columns,
    c("slice_end", "scale")
  )
  for (column in required_text) {
    bad <- is.na(manifest[[column]]) | !nzchar(trimws(manifest[[column]]))
    if (any(bad)) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf("Manifest column `%s` contains an empty value.", column)
      )
    }
  }
  if (any(!manifest$frequency %in% c("M", "Q", "A"))) {
    .nbs_abort(
      "nbs_manifest_error",
      "Direct-series manifest frequencies must be one of M, Q or A."
    )
  }
  valid_period_policies <- c(
    "all_periods",
    "january_not_reported_february_first_period",
    "ytd_january_not_reported_february_combined",
    "current_january_february_not_separately_reported",
    "mixed_jan_feb_combined_then_monthly",
    # Accepted for manifests built against readNBS <= 0.2.0.
    "omit_january",
    "omit_january_february"
  )
  if (any(!manifest$period_policy %in% valid_period_policies)) {
    .nbs_abort(
      "nbs_manifest_error",
      paste0(
        "Manifest period policies must use an explicit readNBS calendar ",
        "policy."
      )
    )
  }
  if (any(
    manifest$period_policy %in% c(
      "ytd_january_not_reported_february_combined",
      "current_january_february_not_separately_reported",
      "mixed_jan_feb_combined_then_monthly",
      "january_not_reported_february_first_period",
      "omit_january",
      "omit_january_february"
    ) &
      manifest$frequency != "M"
  )) {
    .nbs_abort(
      "nbs_manifest_error",
      "January-omission period policies are valid only for monthly series."
    )
  }
  valid_aggregation <- c(
    "single_period", "year_to_date", "jan_feb_combined", "mixed_span",
    "irregular_window"
  )
  if (any(!manifest$aggregation_basis %in% valid_aggregation)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "Manifest aggregation basis must be one of: %s.",
        paste(valid_aggregation, collapse = ", ")
      )
    )
  }
  valid_adjustment <- c(
    "not_seasonally_adjusted",
    "official_seasonally_adjusted",
    "derived_seasonally_adjusted"
  )
  if (any(!manifest$seasonal_adjustment %in% valid_adjustment)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "Manifest seasonal adjustment must be one of: %s.",
        paste(valid_adjustment, collapse = ", ")
      )
    )
  }
  if (any(is.na(manifest$scale) | !is.finite(manifest$scale))) {
    .nbs_abort(
      "nbs_manifest_error",
      "Manifest scale values must be finite numbers."
    )
  }
  uuid_columns <- c("root_uuid", "catalog_uuid", "indicator_uuid")
  for (column in uuid_columns) {
    bad <- !grepl("^[0-9a-f]{32}$", manifest[[column]])
    if (any(bad)) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf("Manifest column `%s` contains an invalid UUID.", column)
      )
    }
  }
  if (anyDuplicated(paste(
    manifest$series_id, manifest$catalog_uuid, manifest$indicator_uuid,
    manifest$slice_start, sep = "\r"
  ))) {
    .nbs_abort(
      "nbs_manifest_error",
      paste(
        "The manifest contains duplicate series/catalogue/indicator",
        "mappings for one slice start."
      )
    )
  }

  starts <- suppressWarnings(as.Date(manifest$slice_start))
  ends <- suppressWarnings(as.Date(ifelse(
    nzchar(manifest$slice_end),
    manifest$slice_end,
    "9999-12-31"
  )))
  defaults <- suppressWarnings(as.Date(manifest$default_start))
  if (anyNA(starts) || anyNA(ends) || anyNA(defaults)) {
    .nbs_abort(
      "nbs_manifest_error",
      "Manifest date bounds must use ISO YYYY-MM-DD dates."
    )
  }
  if (any(starts > ends)) {
    .nbs_abort(
      "nbs_manifest_error",
      "A manifest slice starts after it ends."
    )
  }

  semantic_columns <- c(
    "manifest_version", "title_en", "title_zh", "description_en",
    "description_zh", "frequency", "default_start", "stitch_policy",
    "period_policy", "aggregation_basis", "seasonal_adjustment",
    "series_note_en", "series_note_zh", "unit_en",
    "unit_zh", "scale",
    "geography_code", "geography_en", "geography_zh", "source_url",
    "source_note"
  )
  duplicated_series_rows <- duplicated(manifest$series_id) |
    duplicated(manifest$series_id, fromLast = TRUE)
  duplicated_manifest <- manifest[
    duplicated_series_rows,
    ,
    drop = FALSE
  ]
  series_groups <- split(
    seq_len(nrow(duplicated_manifest)),
    duplicated_manifest$series_id
  )
  for (series_id in names(series_groups)) {
    rows <- duplicated_manifest[
      series_groups[[series_id]],
      ,
      drop = FALSE
    ]
    for (column in semantic_columns) {
      if (length(unique(rows[[column]])) != 1L) {
        .nbs_abort(
          "nbs_manifest_error",
          sprintf(
            "Series `%s` has incompatible `%s` values across slices.",
            series_id, column
          )
        )
      }
    }
    order_index <- order(as.Date(rows$slice_start))
    ordered_starts <- as.Date(rows$slice_start[order_index])
    ordered_ends <- as.Date(ifelse(
      nzchar(rows$slice_end[order_index]),
      rows$slice_end[order_index],
      "9999-12-31"
    ))
    if (length(ordered_starts) > 1L &&
        any(ordered_starts[-1L] <= ordered_ends[-length(ordered_ends)])) {
      .nbs_abort(
        "nbs_manifest_overlap_error",
        sprintf("Manifest slices overlap for series `%s`.", series_id),
        series_id = series_id
      )
    }
  }

  manifest
}

#' Return the bundled NBS transport manifest
#'
#' `nbs_manifest()` is the low-level source-of-truth table for the scalar
#' catalogue. It contains one row per time-bounded transport slice. UUIDs are
#' transport identities; applications should save the stable `series_id`
#' instead. The table is read from package data and validated before it is
#' cached for the current R process.
#'
#' @return A base data frame with these 28 columns, in this order:
#' `manifest_version`, `series_id`, `title_en`, `title_zh`, `description_en`,
#' `description_zh`, `frequency`, `root_uuid`, `catalog_uuid`,
#' `indicator_uuid`, `slice_start`, `slice_end`, `default_start`,
#' `classification_id`, `stitch_policy`, `period_policy`,
#' `aggregation_basis`, `seasonal_adjustment`, `series_note_en`,
#' `series_note_zh`, `unit_en`, `unit_zh`, `scale`, `geography_code`,
#' `geography_en`, `geography_zh`, `source_url`, and `source_note`.
#' Every column is character except numeric `scale`; dates are retained as
#' ISO-8601 character values so that the transport manifest can be compared
#' byte-for-byte across package versions. An empty `slice_end` denotes an
#' open-ended slice. Rows are ordered as stored in the bundled manifest;
#' repeated `series_id` values represent non-overlapping slices with shared
#' semantics.
#'
#' @details This function performs no network request and writes no files.
#' The only side effect is an in-memory package cache. Missing, malformed,
#' overlapping, or semantically inconsistent bundled data raises an
#' `nbs_manifest_error` (or the more specific `nbs_manifest_overlap_error`)
#' condition.
#'
#' @seealso [nbs_catalogue()], [nbs_fetch_series()]
#'
#' @examples
#' manifest <- nbs_manifest()
#' manifest[1, c("series_id", "frequency", "slice_start", "slice_end")]
#' @export
nbs_manifest <- function() {
  .nbs_load_manifest()
}

.nbs_series_family <- function(series_id) {
  parts <- strsplit(series_id, "/", fixed = TRUE)[[1L]]
  if (length(parts) < 4L) "unknown" else tolower(parts[[4L]])
}

.nbs_series_release <- function(family) {
  if (family %in% c("cpi", "ppi", "ppirm")) {
    return("cn_nbs_cpi_ppi")
  }
  if (family == "pmi") {
    return("cn_nbs_pmi")
  }
  if (family == "gdp") {
    return("cn_nbs_gdp")
  }
  if (family == "households") {
    return("cn_nbs_households")
  }
  if (family %in% c(
    "industry", "services", "retail", "fai", "property",
    "unemployment", "employment", "energy"
  )) {
    return("cn_nbs_activity")
  }
  if (family == "industrial_enterprises") {
    return("cn_nbs_industrial_profits")
  }
  if (family == "capacity_utilisation") {
    return("cn_nbs_capacity_utilisation")
  }
  if (family == "migrant_workers") {
    return("cn_nbs_migrant_workers")
  }
  if (family == "population") {
    return("cn_nbs_demographics")
  }
  "cn_nbs_other"
}

#' List the stable local NBS series catalogue
#'
#' The catalogue is the quickest entry point for discovering scalar series.
#' It uses only bundled metadata and never calls the network. Each stable
#' semantic series appears once even when the portal requires several UUID
#' slices to cover its history. Use [nbs_fetch_series()] with the returned
#' `series_id`, rather than using a transport UUID directly.
#'
#' @param language Display language, either `"en"` or `"zh"`.
#' @return A base data frame with one row per stable semantic series and these
#' 33 columns, in this order: `series_id`, `dataset_id`, `release_id`,
#' `family`, `dimension_key`, `dimension_label`, `route`, `provider`, `title`,
#' `title_en`, `title_zh`, `description`, `description_en`,
#' `description_zh`, `series_note`, `series_note_en`, `series_note_zh`,
#' `frequency`, `period_policy`, `aggregation_basis`,
#' `seasonal_adjustment`, `derived_method`, `source_vintage`, `unit`,
#' `unit_en`, `unit_zh`, `geography`, `geography_code`, `available_from`,
#' `available_to`, `manifest_version`, `source_url`, and `source_note`.
#' All columns are character except the two `available_*` columns, which are
#' `Date`. The selected `language` populates `title`, `description`,
#' `series_note`, `unit`, `geography`, and `dimension_label`; bilingual
#' columns are always retained. `available_to` is `NA` for an open-ended
#' series. Portal-backed rows follow the stable manifest order, followed by
#' retained press-series rows in their bundled catalogue order.
#'
#' @details The result is deterministic and is cached in memory for the
#' current R process. It does not write to the user's home directory or to a
#' global cache. A malformed bundled manifest raises `nbs_manifest_error`.
#' `language` is checked with [match.arg()].
#'
#' @seealso [nbs_search()], [nbs_manifest()], [nbs_fetch_series()]
#'
#' @examples
#' catalogue <- nbs_catalogue(language = "en")
#' catalogue[1, c("series_id", "title", "frequency", "unit", "available_from")]
#' @export
nbs_catalogue <- function(language = c("en", "zh")) {
  language <- match.arg(language)
  if (exists(language, envir = .nbs_catalogue_cache, inherits = FALSE)) {
    return(get(language, envir = .nbs_catalogue_cache, inherits = FALSE))
  }
  manifest <- nbs_manifest()
  ids <- unique(manifest$series_id)
  group <- factor(manifest$series_id, levels = ids)
  first <- manifest[match(ids, manifest$series_id), , drop = FALSE]
  starts <- as.numeric(as.Date(manifest$slice_start))
  ends <- as.numeric(as.Date(ifelse(
    nzchar(manifest$slice_end), manifest$slice_end, "9999-12-31"
  )))
  available_from <- as.Date(
    unname(tapply(starts, group, min)), origin = "1970-01-01"
  )
  available_to <- as.Date(
    unname(tapply(ends, group, max)), origin = "1970-01-01"
  )
  open_ended <- unname(tapply(!nzchar(manifest$slice_end), group, any))
  available_to[open_ended] <- as.Date(NA)
  family <- vapply(ids, .nbs_series_family, character(1))
  release_id <- vapply(family, .nbs_series_release, character(1))
  direct <- data.frame(
    series_id = ids,
    dataset_id = ids,
    release_id = release_id,
    family = family,
    dimension_key = "",
    dimension_label = first[[paste0("geography_", language)]],
    route = "portal_uuid_series",
    provider = "nbs",
    title = first[[paste0("title_", language)]],
    title_en = first$title_en,
    title_zh = first$title_zh,
    description = first[[paste0("description_", language)]],
    description_en = first$description_en,
    description_zh = first$description_zh,
    series_note = first[[paste0("series_note_", language)]],
    series_note_en = first$series_note_en,
    series_note_zh = first$series_note_zh,
    frequency = first$frequency,
    period_policy = first$period_policy,
    aggregation_basis = first$aggregation_basis,
    seasonal_adjustment = first$seasonal_adjustment,
    derived_method = "none",
    source_vintage = "latest_revised_database",
    unit = first[[paste0("unit_", language)]],
    unit_en = first$unit_en,
    unit_zh = first$unit_zh,
    geography = first[[paste0("geography_", language)]],
    geography_code = first$geography_code,
    available_from = available_from,
    available_to = available_to,
    manifest_version = first$manifest_version,
    source_url = first$source_url,
    source_note = first$source_note,
    stringsAsFactors = FALSE
  )
  press <- .nbs_press_series_catalogue(language)
  result <- rbind(direct, press)
  rownames(result) <- NULL
  assign(language, result, envir = .nbs_catalogue_cache)
  result
}

#' Search the stable local NBS series catalogue
#'
#' Search is case-insensitive and bilingual: the stable identifier, both
#' titles, both descriptions, and both unit labels are searched. It is
#' deterministic, bounded to at most 100 rows, and performs no network
#' request. An empty query is a short form of [nbs_catalogue()].
#'
#' @param query Search text. Empty text lists the catalogue.
#' @param language Display language, either `"en"` or `"zh"`.
#' @param limit Maximum returned rows, from 1 to 100.
#' @return A data frame with the same 33 columns and classes as
#' [nbs_catalogue()]. Rows are first ordered by relevance (exact identifier,
#' identifier prefix, title prefix, then other matches) and then by
#' `series_id`; `limit` is applied after that ordering. No row names or
#' ranking column are added.
#'
#' @details `query` must be one non-missing character scalar (it may be
#' empty). `limit` must be one finite integer-valued number between 1 and 100.
#' Invalid arguments raise `nbs_argument_error`; `language` is checked with
#' [match.arg()]. The catalogue is read through the same process-local cache
#' as [nbs_catalogue()] and no files or network resources are touched.
#'
#' @seealso [nbs_catalogue()], [nbs_dataset_catalogue()]
#'
#' @examples
#' nbs_search("consumer price", language = "en", limit = 3L)[,
#'   c("series_id", "title", "unit")]
#' @export
nbs_search <- function(query = "", language = c("en", "zh"), limit = 100L) {
  language <- match.arg(language)
  query <- .nbs_scalar_character(query, "query", allow_empty = TRUE)
  limit <- .nbs_scalar_number(limit, "limit", 1, 100, integer = TRUE)
  catalogue <- nbs_catalogue(language)
  if (!nzchar(query)) {
    return(utils::head(catalogue, limit))
  }

  needle <- tolower(query)
  haystacks <- lapply(
    seq_len(nrow(catalogue)),
    function(index) {
      tolower(paste(
        catalogue$series_id[[index]],
        catalogue$title_en[[index]],
        catalogue$title_zh[[index]],
        catalogue$description_en[[index]],
        catalogue$description_zh[[index]],
        catalogue$unit_en[[index]],
        catalogue$unit_zh[[index]]
      ))
    }
  )
  matched <- vapply(haystacks, grepl, logical(1), pattern = needle, fixed = TRUE)
  if (!any(matched)) {
    return(catalogue[FALSE, , drop = FALSE])
  }

  result <- catalogue[matched, , drop = FALSE]
  score <- ifelse(tolower(result$series_id) == needle, 0L,
    ifelse(startsWith(tolower(result$series_id), needle), 1L,
      ifelse(startsWith(tolower(result$title), needle), 2L, 3L)
    )
  )
  result <- result[order(score, result$series_id), , drop = FALSE]
  rownames(result) <- NULL
  utils::head(result, limit)
}
