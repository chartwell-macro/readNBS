.nbs_working_day_shares <- function(working_days, years) {
  if (is.numeric(working_days) && !is.null(names(working_days))) {
    working_days <- data.frame(
      period = names(working_days),
      working_days = as.numeric(working_days),
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(working_days) ||
      !all(c("period", "working_days") %in% names(working_days))) {
    .nbs_abort(
      "nbs_regularisation_error",
      paste0(
        "`working_days` must be a named numeric vector or a data frame ",
        "with `period` and `working_days` columns. readNBS does not guess ",
        "China's holiday-adjusted working calendar."
      )
    )
  }
  period <- as.character(working_days$period)
  count <- suppressWarnings(as.numeric(working_days$working_days))
  if (anyNA(period) || any(!grepl("^\\d{4}-(01|02)$", period)) ||
      anyNA(count) || any(!is.finite(count)) || any(count <= 0)) {
    .nbs_abort(
      "nbs_regularisation_error",
      paste0(
        "Working-day periods must be unique `YYYY-01`/`YYYY-02` values ",
        "with finite positive counts."
      )
    )
  }
  if (anyDuplicated(period)) {
    .nbs_abort(
      "nbs_regularisation_error",
      "`working_days` contains duplicate periods."
    )
  }
  required <- as.vector(rbind(paste0(years, "-01"), paste0(years, "-02")))
  missing <- setdiff(required, period)
  if (length(missing)) {
    .nbs_abort(
      "nbs_regularisation_error",
      sprintf(
        "Working-day counts are missing for: %s.",
        paste(missing, collapse = ", ")
      ),
      missing_periods = missing
    )
  }
  count <- stats::setNames(count, period)
  lapply(years, function(year) {
    jan <- unname(count[[paste0(year, "-01")]])
    feb <- unname(count[[paste0(year, "-02")]])
    c(january = jan / (jan + feb), february = feb / (jan + feb))
  }) |>
    stats::setNames(years)
}

.nbs_month_row <- function(source, date, value, method, source_periods,
                           source_hashes) {
  row <- source
  date <- as.Date(date)
  row$series_id <- paste0(source$series_id[[1L]], "/DERIVED_MONTHLY_FLOW")
  if ("dataset_id" %in% names(row)) {
    row$dataset_id <- row$series_id
  }
  if ("dimension_key" %in% names(row)) {
    row$dimension_key <- nbs_dimension_key(list(
      derived_method = method,
      source_series_id = source$series_id[[1L]]
    ))
  }
  if ("dimension_label" %in% names(row)) {
    row$dimension_label <- "Derived monthly flow"
  }
  if ("route" %in% names(row)) {
    row$route <- "derived_regularisation"
  }
  row$title <- paste0(source$title[[1L]], " \u2014 derived monthly flow")
  row$title_en <- paste0(source$title_en[[1L]], " \u2014 derived monthly flow")
  row$title_zh <- paste0(
    source$title_zh[[1L]],
    "\u2014\u63a8\u7b97\u6708\u5ea6\u6d41\u91cf"
  )
  row$date <- date
  row$period <- format(date, "%Y-%m")
  row$reference_date <- date
  row$reference_period <- row$period
  row$reference_period_start <- date
  row$reference_period_end <- .nbs_period_end(date, "M")
  row$span_months <- 1L
  row$aggregation_basis <- "single_period"
  row$value <- value
  row$raw_value <- NA_character_
  row$is_missing <- is.na(value)
  row$data_status <- "derived_from_latest_revised"
  row$seasonal_adjustment <- "not_seasonally_adjusted"
  row$derived_method <- method
  row$period_policy <- "all_periods"
  row$source_observation_periods <- paste(source_periods, collapse = ",")
  row$source_raw_sha256 <- paste(source_hashes, collapse = ",")
  row$source_vintage <- "derived_from_latest_revised"
  row$raw_sha256 <- .nbs_sha256(charToRaw(paste(
    source_hashes,
    collapse = "|"
  )))
  row
}

#' Convert a reported NBS year-to-date level into monthly flow benchmarks
#'
#' This is an explicit analytical transformation for a monthly, cumulative
#' (year-to-date) NBS series. It never changes the source observations supplied
#' in `data`, and it does not claim to recover official monthly observations.
#' January and February are not observed separately by NBS for the affected
#' releases. The default therefore assigns half of the reported January--
#' February total to each month; later monthly flows are successive differences
#' of the cumulative levels. A working-day-weighted sensitivity is available
#' when the caller supplies China's exact swapped-weekend and public-holiday
#' calendar. Neither method is an NBS or seasonally adjusted series.
#'
#' @param data Observations returned by [nbs_fetch_series()] or
#'   [nbs_fetch_dataset()] for one monthly year-to-date level series. It must
#'   contain the columns `series_id`,
#'   `frequency`, `date`, `period`, `value`, `aggregation_basis`, and
#'   `raw_sha256`. The data must describe exactly one monthly (`"M"`) series,
#'   with `aggregation_basis == "year_to_date"`, a stable ID ending in
#'   `/YTD_LEVEL`, unique periods, and valid consecutive February-to-latest
#'   observations in every retained year. A single incomplete leading calendar
#'   year is omitted when the official archive begins after February; readNBS
#'   never fabricates its missing cumulative observations.
#' @param method Allocation method. The default, `"equal_split_benchmark"`,
#'   allocates one half of the January--February cumulative value to each month.
#'   `"working_day_share"` allocates it using `working_days`.
#'   `"equal_split_legacy"` and `"working_days"` are backwards-compatible
#'   aliases for the two canonical methods; `"equal_split_rba"` is a deprecated
#'   compatibility alias for `"equal_split_benchmark"`. Aliases are accepted
#'   without changing the source data; new code should use the canonical names.
#' @param working_days Required when `method = "working_day_share"`; ignored by
#'   the other methods. Supply either a named numeric vector, such as
#'   `c("2020-01" = 23, "2020-02" = 20)`, or a data frame with columns
#'   `period` (the required `YYYY-01` and `YYYY-02` values) and `working_days`.
#'   Counts must be finite and positive. readNBS deliberately does not infer
#'   China's swapped-weekend and public-holiday calendar.
#' @return A base `data.frame` with `stringsAsFactors = FALSE` and one row per
#'   derived month, ordered oldest first. All source columns are retained where
#'   possible, while `date`, `period`, `reference_date`,
#'   `reference_period_start`, and `reference_period_end` describe one month;
#'   `span_months` is integer `1L`; and `aggregation_basis` is
#'   `"single_period"`. The derived `series_id` ends in
#'   `/DERIVED_MONTHLY_FLOW`, `value` is numeric, `raw_value` is `NA_character_`,
#'   and `is_missing` records whether the derived value is missing. The
#'   provenance columns `derived_method`, `source_observation_periods`,
#'   `source_raw_sha256`, `source_vintage`, `data_status`, and
#'   `source_omitted_observation_periods` identify the calculation and its
#'   source periods. `source_omitted_observation_periods` is a comma-separated
#'   character value repeated on each row (empty when no leading year was
#'   omitted). No custom attributes are added.
#' @section Conditions and side effects:
#' An unsupported `method` is reported by [match.arg()]. Other invalid input,
#' unsupported series geometry, duplicate periods, missing working-day counts,
#' and non-consecutive retained years signal a structured
#' `nbs_regularisation_error` condition; malformed date values can also
#' propagate a base date-conversion error. The function emits no warning,
#' makes no network request, writes no file, and does not modify a client or a
#' cache. It is an analytical transformation, not a replacement for the
#' official NBS monthly series or seasonal adjustment.
#' @family transformations
#' @seealso [nbs_fetch_series()], [nbs_fetch_dataset()],
#'   [nbs_comparison_index_to_percent()]
#' @examples
#' # This retained official property-development release is local. The
#' # injected transport would fail if the example tried to use the network.
#' property_series <- paste0(
#'   "NBS/CN/M/PROPERTY/FUNDING/DEPOSITS_ADVANCE_RECEIPTS/YTD_LEVEL"
#' )
#' offline_client <- nbs_client(
#'   transport = function(request) {
#'     stop("The regularisation example must not contact the network.")
#'   },
#'   retries = 0L
#' )
#' property <- nbs_fetch_dataset(
#'   "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
#'   start = "2020-02", end = "2020-04", client = offline_client
#' )
#' property <- property[property$series_id == property_series, , drop = FALSE]
#' monthly <- nbs_regularise_monthly(property)
#' stopifnot(
#'   identical(monthly$period, c("2020-01", "2020-02", "2020-03", "2020-04")),
#'   isTRUE(all.equal(monthly$value, c(2801.5, 2801.5, 3939, 4448))),
#'   all(monthly$derived_method == "equal_split_benchmark")
#' )
#' monthly[c("period", "value", "source_observation_periods", "derived_method")]
#' @export
nbs_regularise_monthly <- function(
    data,
    method = c(
      "equal_split_benchmark", "working_day_share",
      "equal_split_rba", "equal_split_legacy", "working_days"
    ),
    working_days = NULL) {
  method <- match.arg(method)
  if (method == "working_days") {
    method <- "working_day_share"
  } else if (method %in% c("equal_split_rba", "equal_split_legacy")) {
    method <- "equal_split_benchmark"
  }
  required <- c(
    "series_id", "frequency", "date", "period", "value",
    "aggregation_basis", "raw_sha256"
  )
  if (!is.data.frame(data) || any(!required %in% names(data))) {
    .nbs_abort(
      "nbs_regularisation_error",
      sprintf(
        "`data` must be an NBS observation data frame containing: %s.",
        paste(required, collapse = ", ")
      )
    )
  }
  if (!nrow(data)) {
    .nbs_abort(
      "nbs_regularisation_error",
      "`data` must contain at least one observation."
    )
  }
  if (length(unique(data$series_id)) != 1L ||
      !identical(unique(data$frequency), "M")) {
    .nbs_abort(
      "nbs_regularisation_error",
      "`data` must contain exactly one monthly NBS series."
    )
  }
  if (any(data$aggregation_basis != "year_to_date")) {
    .nbs_abort(
      "nbs_regularisation_error",
      "Only year-to-date observations can be regularised into monthly flows."
    )
  }
  if (!grepl("/YTD_LEVEL$", unique(data$series_id))) {
    .nbs_abort(
      "nbs_regularisation_error",
      paste0(
        "Regularisation is defined only for stable IDs ending in ",
        "`/YTD_LEVEL`; percentages and indexes cannot be disaggregated."
      )
    )
  }
  data <- data[order(as.Date(data$date)), , drop = FALSE]
  if (anyDuplicated(data$period)) {
    .nbs_abort(
      "nbs_regularisation_error",
      "`data` contains duplicate periods."
    )
  }
  years <- unique(format(as.Date(data$date), "%Y"))

  # Some official portal leaves begin part-way through their first historical
  # year (for example, office completions begins in October 2000) and become
  # complete February-to-December YTD series from the following year. The
  # cumulative level at that mid-year boundary cannot identify the missing
  # monthly flows, so omit the entire boundary year rather than inventing them
  # or rejecting every later complete year.
  omitted_periods <- character()
  first_year <- years[[1L]]
  first_block <- data[
    format(as.Date(data$date), "%Y") == first_year,
    ,
    drop = FALSE
  ]
  first_month <- as.integer(format(min(as.Date(first_block$date)), "%m"))
  if (!is.na(first_month) && first_month > 2L) {
    omitted_periods <- as.character(first_block$period)
    data <- data[
      format(as.Date(data$date), "%Y") != first_year,
      ,
      drop = FALSE
    ]
    years <- unique(format(as.Date(data$date), "%Y"))
  }
  if (!nrow(data)) {
    .nbs_abort(
      "nbs_regularisation_error",
      sprintf(
        paste0(
          "The official archive contains no complete February-to-latest ",
          "YTD year after omitting its incomplete leading year %s."
        ),
        first_year
      ),
      year = first_year,
      omitted_periods = omitted_periods
    )
  }

  # Validate all retained years before allocating January-February shares so a
  # working-day caller is never asked for calendars belonging only to an
  # omitted boundary year.
  for (year in years) {
    block <- data[format(as.Date(data$date), "%Y") == year, , drop = FALSE]
    expected <- .nbs_period_sequence(
      as.Date(paste0(year, "-02-01")),
      max(as.Date(block$date)),
      "M"
    )
    if (!identical(as.Date(block$date), expected)) {
      .nbs_abort(
        "nbs_regularisation_error",
        sprintf(
          "Year %s must begin in February and contain consecutive YTD periods.",
          year
        ),
        year = year
      )
    }
  }

  shares <- if (method == "working_day_share") {
    .nbs_working_day_shares(working_days, years)
  } else {
    stats::setNames(
      rep(list(c(january = 0.5, february = 0.5)), length(years)),
      years
    )
  }

  output <- list()
  for (year in years) {
    block <- data[format(as.Date(data$date), "%Y") == year, , drop = FALSE]
    feb <- block[1L, , drop = FALSE]
    combined <- feb$value[[1L]]
    output[[length(output) + 1L]] <- .nbs_month_row(
      feb,
      paste0(year, "-01-01"),
      combined * shares[[year]][["january"]],
      method,
      feb$period,
      feb$raw_sha256
    )
    output[[length(output) + 1L]] <- .nbs_month_row(
      feb,
      paste0(year, "-02-01"),
      combined * shares[[year]][["february"]],
      method,
      feb$period,
      feb$raw_sha256
    )
    if (nrow(block) > 1L) {
      for (index in 2:nrow(block)) {
        current <- block[index, , drop = FALSE]
        previous <- block[index - 1L, , drop = FALSE]
        value <- current$value[[1L]] - previous$value[[1L]]
        output[[length(output) + 1L]] <- .nbs_month_row(
          current,
          current$date,
          value,
          method,
          c(previous$period, current$period),
          c(previous$raw_sha256, current$raw_sha256)
        )
      }
    }
  }
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result$source_omitted_observation_periods <- paste(
    omitted_periods,
    collapse = ","
  )
  result
}

#' Convert an NBS comparison index into a year-on-year percentage change
#'
#' China NBS publishes many CPI, PPI, and purchasing-price series as comparison
#' indexes whose base is the same month of the previous year equal to 100. For
#' those observations, the economically familiar year-on-year percentage change
#' is exactly the published index minus 100. This function applies only that
#' identity; it does not rebase, interpolate, or seasonally adjust data.
#'
#' @param data A data frame returned by [nbs_fetch_series()] or
#'   [nbs_fetch_dataset()] containing one or more monthly observations. It must
#'   contain `series_id`, `frequency`, `date`, `period`, and `value`; each ID
#'   must end in `/YOY_INDEX`, `frequency` must be `"M"`, and `unit_en` (or,
#'   when `unit_en` is absent, `unit`) must be exactly
#'   `"Index (same month previous year=100)"`. Dates must be valid and unique
#'   within each series. Missing values are allowed; infinite values are not.
#' @return A base `data.frame` in the input row order and with no custom
#'   attributes. The input source columns are retained. `value` is numeric and
#'   equals the numeric coercion of the original index minus 100 (with `NA`
#'   preserved), `source_index_value` stores that coerced index, and `unit` is
#'   set to `"Percent"`. Nonnumeric source values are treated as missing, as in
#'   the existing observation parser.
#'   Existing `unit_en` and `unit_zh` columns become `"Percent"` and `"%"`,
#'   respectively; existing `raw_value` and `raw_sha256` remain unchanged.
#'   `derived_method` is `"comparison_index_minus_100"`,
#'   `source_observation_periods` copies `period`, and
#'   `source_raw_sha256` copies `raw_sha256` (or contains empty strings when the
#'   input has no `raw_sha256` column). If present, `data_status` is marked
#'   `"derived_from_latest_revised"`; an English title ending in
#'   `"year-on-year"` is relabelled as a percentage change, and an existing
#'   Chinese title receives the corresponding percentage-change suffix.
#' @section Conditions and side effects:
#' Missing required columns, non-monthly or non-`/YOY_INDEX` series, an
#' unexpected official unit, duplicate dates, and infinite values signal a
#' structured `nbs_regularisation_error` condition. Malformed date strings can
#' instead propagate a base date-conversion error. The function suppresses
#' numeric-coercion warnings and treats nonnumeric values as missing; it emits
#' no package warning, makes no network request, writes no file, and does not
#' modify the source object or any client/cache.
#' @family transformations
#' @seealso [nbs_fetch_series()], [nbs_fetch_dataset()],
#'   [nbs_regularise_monthly()]
#' @examples
#' cpi_file <- system.file(
#'   "extdata", "examples", "cpi-2026-june.json", package = "readNBS"
#' )
#' cpi_transport <- function(request) {
#'   stopifnot(identical(request$body$dts[[1L]], "202606MM-202606MM"))
#'   list(
#'     status = 200L,
#'     headers = list(`content-type` = "application/json"),
#'     body = paste(readLines(cpi_file, encoding = "UTF-8", warn = FALSE),
#'       collapse = "\n")
#'   )
#' }
#' cpi_client <- nbs_client(transport = cpi_transport, retries = 0L)
#' cpi <- nbs_fetch_series(
#'   "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
#'   start = "2026-06", end = "2026-06", client = cpi_client
#' )
#' converted <- nbs_comparison_index_to_percent(cpi)
#' stopifnot(
#'   identical(cpi$value, converted$source_index_value),
#'   isTRUE(all.equal(converted$value, 1)),
#'   identical(converted$derived_method, "comparison_index_minus_100")
#' )
#' converted[c("period", "value", "source_index_value", "derived_method")]
#' @export
nbs_comparison_index_to_percent <- function(data) {
  required <- c("series_id", "frequency", "date", "period", "value")
  if (!is.data.frame(data) || any(!required %in% names(data))) {
    .nbs_abort(
      "nbs_regularisation_error",
      sprintf(
        "`data` must be an NBS observation data frame containing: %s.",
        paste(required, collapse = ", ")
      )
    )
  }
  if (!nrow(data)) {
    .nbs_abort(
      "nbs_regularisation_error",
      "`data` must contain at least one observation."
    )
  }
  if (any(as.character(data$frequency) != "M") ||
      any(!grepl("/YOY_INDEX$", as.character(data$series_id)))) {
    .nbs_abort(
      "nbs_regularisation_error",
      paste0(
        "This conversion is defined only for monthly NBS stable IDs ending ",
        "in `/YOY_INDEX`."
      )
    )
  }
  english_unit <- if ("unit_en" %in% names(data)) {
    as.character(data$unit_en)
  } else if ("unit" %in% names(data)) {
    as.character(data$unit)
  } else {
    rep("", nrow(data))
  }
  expected_unit <- "Index (same month previous year=100)"
  if (any(is.na(english_unit) | english_unit != expected_unit)) {
    .nbs_abort(
      "nbs_regularisation_error",
      sprintf(
        "Every observation must use the official `%s` unit.",
        expected_unit
      )
    )
  }
  dates <- suppressWarnings(as.Date(data$date))
  values <- suppressWarnings(as.numeric(data$value))
  if (anyNA(dates) || anyDuplicated(paste(data$series_id, dates)) ||
      any(is.infinite(values))) {
    .nbs_abort(
      "nbs_regularisation_error",
      paste0(
        "Comparison-index observations require valid dates, unique ",
        "series/date identities and finite numeric values or missing values."
      )
    )
  }

  source_hash <- if ("raw_sha256" %in% names(data)) {
    as.character(data$raw_sha256)
  } else {
    rep("", nrow(data))
  }
  source_value <- values
  data$value <- source_value - 100
  data$source_index_value <- source_value
  data$unit <- "Percent"
  if ("unit_en" %in% names(data)) data$unit_en <- "Percent"
  if ("unit_zh" %in% names(data)) data$unit_zh <- "%"
  if ("title" %in% names(data)) {
    data$title <- sub(
      "year-on-year$",
      "year-on-year percentage change",
      as.character(data$title),
      ignore.case = TRUE
    )
  }
  if ("title_en" %in% names(data)) {
    data$title_en <- sub(
      "year-on-year$",
      "year-on-year percentage change",
      as.character(data$title_en),
      ignore.case = TRUE
    )
  }
  if ("title_zh" %in% names(data)) {
    data$title_zh <- paste0(
      as.character(data$title_zh),
      "\uff08\u540c\u6bd4\u6da8\u8dcc\u5e45\uff09"
    )
  }
  data$derived_method <- "comparison_index_minus_100"
  data$source_observation_periods <- as.character(data$period)
  data$source_raw_sha256 <- source_hash
  if ("data_status" %in% names(data)) {
    data$data_status <- "derived_from_latest_revised"
  }
  data
}
