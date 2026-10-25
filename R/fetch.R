.nbs_slice_intersections <- function(slices, start, end) {
  slice_start <- as.Date(slices$slice_start)
  slice_end <- as.Date(ifelse(
    nzchar(slices$slice_end),
    slices$slice_end,
    "9999-12-31"
  ))
  keep <- slice_start <= end & slice_end >= start
  slices <- slices[keep, , drop = FALSE]
  if (!nrow(slices)) {
    return(list())
  }
  slices <- slices[order(as.Date(slices$slice_start)), , drop = FALSE]
  intersections <- lapply(seq_len(nrow(slices)), function(index) {
    row <- slices[index, , drop = FALSE]
    row_start <- as.Date(row$slice_start)
    row_end <- as.Date(ifelse(
      nzchar(row$slice_end),
      row$slice_end,
      "9999-12-31"
    ))
    list(
      slice = row,
      start = if (start > row_start) start else row_start,
      end = if (end < row_end) end else row_end
    )
  })

  frequency <- slices$frequency[[1L]]
  policy <- unique(slices$period_policy)
  if (length(policy) != 1L) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("Series `%s` has incompatible period policies.", slices$series_id[[1L]]),
      series_id = slices$series_id[[1L]]
    )
  }
  expected <- .nbs_expected_period_sequence(start, end, frequency, policy)
  covered <- vapply(expected, function(date) {
    any(vapply(intersections, function(intersection) {
      date >= intersection$start && date <= intersection$end
    }, logical(1L)))
  }, logical(1L))
  if (any(!covered)) {
    .nbs_abort(
      "nbs_manifest_gap_error",
      sprintf(
        "The manifest does not cover `%s` for: %s.",
        slices$series_id[[1L]],
        paste(as.character(expected[!covered]), collapse = ", ")
      ),
      series_id = slices$series_id[[1L]],
      missing_dates = expected[!covered]
    )
  }
  intersections
}

.nbs_fetch_slice <- function(client, slice, start, end, language) {
  body <- list(
    cid = slice$catalog_uuid,
    indicatorIds = list(slice$indicator_uuid),
    daCatalogId = "",
    das = list(list(
      text = if (language == "en") slice$geography_en else slice$geography_zh,
      value = slice$geography_code
    )),
    showType = "1",
    dts = list(.nbs_encode_range(start, end, slice$frequency)),
    rootId = slice$root_uuid
  )
  response <- .nbs_observation_request(client, body, language)
  result <- .nbs_parse_observations(response, slice, language)
  if (any(result$date < start | result$date > end)) {
    .nbs_abort(
      "nbs_period_schema_error",
      sprintf(
        "NBS returned observations outside the requested range %s to %s.",
        start, end
      ),
      series_id = slice$series_id,
      raw_sha256 = response$raw_sha256
    )
  }
  expected_dates <- .nbs_expected_period_sequence(
    start,
    end,
    slice$frequency,
    slice$period_policy
  )
  result <- result[result$date %in% expected_dates, , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Fetch a stable NBS series
#'
#' Fetches exact official source values for one stable series ID and stitches
#' only the time slices explicitly mapped by the bundled manifest. Manifest-
#' backed series are requested from the official NBS observation service; a
#' small set of documented press-table series is served from retained official
#' history when the requested range is covered locally. The returned database
#' history represents the latest/revised database where that is the source
#' route, rather than silently pretending to be a first-release vintage.
#'
#' A complete locally retained range performs no network request. If a retained
#' press-table series is requested beyond its local archive, readNBS retrieves
#' the official release table through `client` and combines it with the retained
#' rows. No credentials are required. For ordinary manifest-backed requests,
#' `use_cache = TRUE` reuses a response already held in the client's bounded
#' in-memory cache; the cache is not written to disk.
#'
#' @param series_id A non-empty stable series ID returned by [nbs_catalogue()].
#'   IDs are case-sensitive and identify both the measure and its source
#'   classification/coverage.
#' @param start Optional first observation period. Accepted character forms
#'   include ISO dates, `YYYY-MM`, `YYYY-Qn`, and `YYYY`, as appropriate for the
#'   series frequency. If omitted, the series' manifest default start is used.
#' @param end Optional final observation period in the same forms as `start`.
#'   For an ordinary manifest-backed series, omission uses the most recently
#'   completed period according to `client$now()`. For a retained press-table
#'   series, omission uses the latest period available in its bundled or
#'   official release table.
#' @param language Portal and display language, either `"en"` (the default) or
#'   `"zh"`. It controls request routing and the language-specific title,
#'   geography, unit, and note fields in the result.
#' @param client An object returned by [nbs_client()]. Its timeout, retry,
#'   endpoint, clock, and transport settings govern network-backed work.
#' @param use_cache A single logical value; when `TRUE` (the default), use and
#'   populate the client's bounded response cache for manifest-backed
#'   observation requests. `FALSE` bypasses that cache. Retained press-table
#'   archive reads are already local and are not response-cache entries.
#' @return A base `data.frame` with `stringsAsFactors = FALSE`, one row per
#'   returned period, sorted in ascending `date` order with no duplicate
#'   `period` values. The result has no custom provenance attributes; provenance
#'   is explicit in these columns:
#'   `series_id`, `dataset_id`, `release_id`, `family`, `dimension_key`,
#'   `dimension_label`, `route`, `provider`, `title`, `title_en`, `title_zh`,
#'   `series_note`, `series_note_en`, `series_note_zh`, `date`, `period`,
#'   `reference_date`, `reference_period`, `reference_period_start`,
#'   `reference_period_end`, `span_months`, `aggregation_basis`, `release_date`,
#'   `vintage_date`, `data_status`, `seasonal_adjustment`, `derived_method`,
#'   `source_vintage`, `source_observation_periods`, `source_raw_sha256`,
#'   `frequency`, `value`, `raw_value`, `is_missing`, `unit`, `unit_en`,
#'   `unit_zh`, `raw_unit`, `scale`, `geography_code`, `geography`,
#'   `raw_geography`, `catalog_uuid`, `indicator_uuid`, `root_uuid`,
#'   `classification_id`, `stitch_policy`, `period_policy`, `slice_start`,
#'   `slice_end`, `retrieved_at`, `source_url`, `source_note`, `raw_sha256`,
#'   `manifest_version`, `parser_version`, and `package_version`.
#'   `date`, `reference_date`, `reference_period_start`,
#'   `reference_period_end`, `release_date`, `vintage_date`, `slice_start`, and
#'   `slice_end` are `Date`; `retrieved_at` is `POSIXct`; `span_months` is
#'   integer; `value` and `scale` are numeric; `is_missing` is logical; all
#'   remaining columns are character. `value` is the scaled numeric value,
#'   while `raw_value` preserves the source token. Missing or suppressed source
#'   values are represented by `value = NA_real_` and `is_missing = TRUE`, never
#'   by zero. SHA-256 fields contain the raw-source hash when available.
#' @section Conditions and side effects:
#' Invalid scalar, cache, or client arguments signal `nbs_argument_error`; an
#' unsupported `language` is reported by [match.arg()]. Other common
#' conditions include `nbs_unknown_series_error`, `nbs_period_error`,
#' `nbs_period_limit_error`, `nbs_range_error`, `nbs_manifest_error`,
#' `nbs_manifest_gap_error`, `nbs_period_schema_error`,
#' `nbs_period_overlap_error`, and `nbs_incomplete_period_error`. A
#' network-backed route can additionally signal `nbs_endpoint_error` and the
#' HTTP/JSON/API conditions documented by [nbs_client()]; retained press-table
#' routes can signal `nbs_press_schema_error` or
#' `nbs_dataset_cardinality_error` when a source no longer satisfies its
#' contract. A network-backed call may retry according to `client`; it has no
#' filesystem side effect. Local archive and manifest data are loaded lazily
#' into package memory.
#' @family retrieval
#' @seealso [nbs_fetch_dataset()], [nbs_catalogue()], [nbs_client()],
#'   [nbs_clear_cache()]
#' @examples
#' # This two-period example is served entirely from the retained official
#' # revision history bundled with readNBS; it does not contact the NBS portal.
#' activity <- nbs_fetch_series(
#'   "NBS/CN/M/INDUSTRY/VALUE_ADDED/MOM_PERCENT_OFFICIAL_SA",
#'   start = "2025-04",
#'   end = "2025-05"
#' )
#' activity[c("period", "value", "source_vintage")]
#' @export
nbs_fetch_series <- function(
    series_id,
    start = NULL,
    end = NULL,
    language = c("en", "zh"),
    client = nbs_client(),
    use_cache = TRUE) {
  series_id <- .nbs_scalar_character(series_id, "series_id")
  language <- match.arg(language)
  .nbs_validate_client(client)
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    .nbs_abort(
      "nbs_argument_error",
      "`use_cache` must be TRUE or FALSE."
    )
  }

  manifest <- nbs_manifest()
  slices <- manifest[manifest$series_id == series_id, , drop = FALSE]
  if (!nrow(slices)) {
    press_result <- .nbs_fetch_press_series(
      series_id = series_id,
      start = start,
      end = end,
      language = language,
      client = client
    )
    if (!is.null(press_result)) {
      return(press_result)
    }
    .nbs_abort(
      "nbs_unknown_series_error",
      sprintf(
        "Unknown NBS series `%s`. Use `nbs_catalogue()` for stable IDs.",
        series_id
      ),
      series_id = series_id
    )
  }
  frequency <- slices$frequency[[1L]]
  start_date <- if (is.null(start)) {
    as.Date(slices$default_start[[1L]])
  } else {
    .nbs_period_date(start, frequency, "start")
  }
  end_date <- if (is.null(end)) {
    .nbs_previous_completed_period(as.Date(client$now()), frequency)
  } else {
    .nbs_period_date(end, frequency, "end")
  }
  if (start_date > end_date) {
    .nbs_abort(
      "nbs_period_error",
      sprintf("`start` (%s) is after `end` (%s).", start_date, end_date)
    )
  }
  period_count <- .nbs_period_count(start_date, end_date, frequency)
  if (period_count > client$max_periods) {
    .nbs_abort(
      "nbs_period_limit_error",
      sprintf(
        "The request spans %d periods; the client limit is %d.",
        period_count, client$max_periods
      ),
      period_count = period_count,
      max_periods = client$max_periods
    )
  }

  intersections <- .nbs_slice_intersections(slices, start_date, end_date)
  if (!length(intersections)) {
    .nbs_abort(
      "nbs_range_error",
      sprintf(
        "The bundled manifest has no coverage for `%s` from %s to %s.",
        series_id, start_date, end_date
      ),
      series_id = series_id
    )
  }

  cache_key <- digest::digest(
    list(
      series_id = series_id,
      start = as.character(start_date),
      end = as.character(end_date),
      language = language,
      manifest_version = unique(slices$manifest_version),
      base = .nbs_base_url(client, language),
      observation_path = client$observation_path
    ),
    algo = "sha256"
  )
  if (use_cache && client$cache_max_entries > 0L && client$cache_ttl > 0) {
    cached <- .nbs_cache_get(client, cache_key)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  frames <- lapply(intersections, function(intersection) {
    expected <- .nbs_expected_period_sequence(
      intersection$start,
      intersection$end,
      intersection$slice$frequency,
      intersection$slice$period_policy
    )
    if (!length(expected)) {
      return(NULL)
    }
    .nbs_fetch_slice(
      client = client,
      slice = intersection$slice,
      start = intersection$start,
      end = intersection$end,
      language = language
    )
  })
  frames <- Filter(Negate(is.null), frames)
  result <- if (length(frames)) {
    do.call(rbind, frames)
  } else {
    .nbs_empty_observations()
  }
  if (nrow(result)) {
    duplicate_periods <- duplicated(result$period) |
      duplicated(result$period, fromLast = TRUE)
    if (any(duplicate_periods)) {
      .nbs_abort(
        "nbs_period_overlap_error",
        sprintf(
          "Stitched NBS responses overlap for periods: %s.",
          paste(unique(result$period[duplicate_periods]), collapse = ", ")
        ),
        series_id = series_id
      )
    }
    result <- result[order(result$date), , drop = FALSE]
    rownames(result) <- NULL
  }
  period_policy <- unique(slices$period_policy)
  if (length(period_policy) != 1L) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("Series `%s` has incompatible period policies.", series_id),
      series_id = series_id
    )
  }
  expected_dates <- .nbs_expected_period_sequence(
    start_date,
    end_date,
    frequency,
    period_policy
  )
  missing_dates <- setdiff(expected_dates, result$date)
  if (length(missing_dates)) {
    .nbs_abort(
      "nbs_incomplete_period_error",
      sprintf(
        "NBS omitted %d requested period(s) for `%s`: %s.",
        length(missing_dates),
        series_id,
        paste(as.character(missing_dates), collapse = ", ")
      ),
      series_id = series_id,
      missing_dates = missing_dates
    )
  }
  if (use_cache && client$cache_max_entries > 0L && client$cache_ttl > 0) {
    .nbs_cache_set(client, cache_key, result)
  }
  result
}
