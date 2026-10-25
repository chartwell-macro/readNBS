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
#' Fetches exact official source values and stitches only time slices explicitly
#' mapped by the bundled manifest. Returned database history is latest/revised
#' history, not a first-release vintage.
#'
#' @param series_id A stable ID returned by [nbs_catalogue()].
#' @param start Optional first period. Accepted forms include ISO dates,
#'   `YYYY-MM`, `YYYY-Qn`, and `YYYY`, as appropriate for the series.
#' @param end Optional final period in the same accepted forms.
#'   When omitted, the most recently completed calendar period is used.
#' @param language Portal and display language, either `"en"` or `"zh"`.
#' @param client An object returned by [nbs_client()].
#' @param use_cache Whether to use the client's bounded response cache.
#' @return A rich long-form observation data frame, sorted oldest first.
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
