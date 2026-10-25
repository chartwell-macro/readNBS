.nbs_period_date <- function(x, frequency, bound = c("start", "end")) {
  bound <- match.arg(bound)
  frequency <- match.arg(frequency, c("M", "Q", "A"))
  if (inherits(x, "POSIXt")) {
    x <- as.Date(x)
  }
  if (inherits(x, "Date")) {
    if (length(x) != 1L || is.na(x)) {
      .nbs_abort(
        "nbs_period_error",
        "Period bounds must contain exactly one non-missing date."
      )
    }
    date <- x
  } else {
    x <- .nbs_scalar_character(as.character(x), "period bound")
    if (frequency == "M" && grepl("^\\d{4}$", x)) {
      x <- paste0(x, if (bound == "start") "-01-01" else "-12-01")
    } else if (frequency == "M" &&
               grepl("^\\d{4}-(0[1-9]|1[0-2])$", x)) {
      x <- paste0(x, "-01")
    } else if (frequency == "Q" &&
               grepl("^\\d{4}$", x)) {
      x <- paste0(x, if (bound == "start") "-Q1" else "-Q4")
    }
    if (frequency == "Q" &&
        grepl("^\\d{4}-?Q[1-4]$", x, ignore.case = TRUE)) {
      year <- as.integer(substr(x, 1L, 4L))
      quarter <- as.integer(sub("^.*[Qq]", "", x))
      return(as.Date(sprintf(
        "%04d-%02d-01",
        year,
        (quarter - 1L) * 3L + 1L
      )))
    }
    if (frequency == "A" && grepl("^\\d{4}$", x)) {
      x <- paste0(x, "-01-01")
    }
    date <- suppressWarnings(as.Date(x))
    if (is.na(date)) {
      .nbs_abort(
        "nbs_period_error",
        sprintf("Could not parse `%s` as a %s period bound.", x, frequency)
      )
    }
  }
  .nbs_floor_date(date, frequency)
}

.nbs_floor_date <- function(date, frequency) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  if (frequency == "M") {
    return(as.Date(sprintf("%04d-%02d-01", year, month)))
  }
  if (frequency == "Q") {
    quarter_month <- ((month - 1L) %/% 3L) * 3L + 1L
    return(as.Date(sprintf("%04d-%02d-01", year, quarter_month)))
  }
  as.Date(sprintf("%04d-01-01", year))
}

.nbs_previous_completed_period <- function(date, frequency) {
  current <- .nbs_floor_date(as.Date(date), frequency)
  year <- as.integer(format(current, "%Y"))
  month <- as.integer(format(current, "%m"))
  if (frequency == "A") {
    return(as.Date(sprintf("%04d-01-01", year - 1L)))
  }

  decrement <- if (frequency == "M") 1L else 3L
  absolute_month <- year * 12L + month - 1L - decrement
  previous_year <- absolute_month %/% 12L
  previous_month <- absolute_month %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", previous_year, previous_month))
}

.nbs_next_period <- function(date, frequency) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  if (frequency == "A") {
    return(as.Date(sprintf("%04d-01-01", year + 1L)))
  }
  increment <- if (frequency == "M") 1L else 3L
  absolute_month <- year * 12L + month - 1L + increment
  next_year <- absolute_month %/% 12L
  next_month <- absolute_month %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", next_year, next_month))
}

.nbs_period_sequence <- function(start, end, frequency) {
  dates <- as.Date(character())
  cursor <- as.Date(start)
  end <- as.Date(end)
  while (cursor <= end) {
    dates <- c(dates, cursor)
    cursor <- .nbs_next_period(cursor, frequency)
  }
  dates
}

.nbs_expected_period_sequence <- function(
    start,
    end,
    frequency,
    period_policy = "all_periods") {
  dates <- .nbs_period_sequence(start, end, frequency)
  if (period_policy %in% c(
    "ytd_january_not_reported_february_combined",
    "january_not_reported_february_first_period",
    "mixed_jan_feb_combined_then_monthly",
    "omit_january"
  )) {
    return(dates[format(dates, "%m") != "01"])
  }
  if (period_policy %in% c(
    "current_january_february_not_separately_reported",
    "omit_january_february"
  )) {
    return(dates[!format(dates, "%m") %in% c("01", "02")])
  }
  dates
}

.nbs_period_end <- function(date, frequency) {
  .nbs_next_period(as.Date(date), frequency) - 1
}

.nbs_observation_span <- function(
    date,
    frequency,
    period_policy,
    aggregation_basis = "single_period") {
  date <- as.Date(date)
  if (aggregation_basis == "year_to_date") {
    end <- .nbs_period_end(date, frequency)
    return(list(
      start = as.Date(sprintf("%s-01-01", format(date, "%Y"))),
      end = end,
      span_months = as.integer(format(end, "%m")),
      aggregation_basis = "year_to_date"
    ))
  }
  if (frequency == "A") {
    return(list(
      start = date,
      end = .nbs_period_end(date, frequency),
      span_months = 12L,
      aggregation_basis = aggregation_basis
    ))
  }
  if (frequency == "Q") {
    return(list(
      start = date,
      end = .nbs_period_end(date, frequency),
      span_months = 3L,
      aggregation_basis = aggregation_basis
    ))
  }

  month <- as.integer(format(date, "%m"))
  if (period_policy == "mixed_jan_feb_combined_then_monthly" &&
      month == 2L) {
    return(list(
      start = as.Date(sprintf("%s-01-01", format(date, "%Y"))),
      end = .nbs_period_end(date, frequency),
      span_months = 2L,
      aggregation_basis = "jan_feb_combined"
    ))
  }
  list(
    start = date,
    end = .nbs_period_end(date, frequency),
    span_months = 1L,
    aggregation_basis = aggregation_basis
  )
}

.nbs_period_count <- function(start, end, frequency) {
  start_year <- as.integer(format(start, "%Y"))
  end_year <- as.integer(format(end, "%Y"))
  if (frequency == "A") {
    return(end_year - start_year + 1L)
  }
  start_month <- as.integer(format(start, "%m"))
  end_month <- as.integer(format(end, "%m"))
  months <- (end_year - start_year) * 12L + end_month - start_month
  if (frequency == "M") {
    return(months + 1L)
  }
  months %/% 3L + 1L
}

.nbs_encode_range <- function(start, end, frequency) {
  if (frequency == "M") {
    return(sprintf(
      "%sMM-%sMM",
      format(start, "%Y%m"),
      format(end, "%Y%m")
    ))
  }
  if (frequency == "Q") {
    start_q <- (as.integer(format(start, "%m")) - 1L) %/% 3L + 1L
    end_q <- (as.integer(format(end, "%m")) - 1L) %/% 3L + 1L
    return(sprintf(
      "%s%02dSS-%s%02dSS",
      format(start, "%Y"), start_q,
      format(end, "%Y"), end_q
    ))
  }
  sprintf("%sYY-%sYY", format(start, "%Y"), format(end, "%Y"))
}

.nbs_decode_period <- function(code, frequency) {
  code <- .nbs_scalar_character(code, "NBS period code")
  if (frequency == "M") {
    match <- regexec("^(\\d{4})(0[1-9]|1[0-2])MM$", code)
    parts <- regmatches(code, match)[[1L]]
    if (!length(parts)) {
      .nbs_abort(
        "nbs_period_schema_error",
        sprintf("Unexpected monthly NBS period code `%s`.", code),
        period_code = code
      )
    }
    date <- as.Date(sprintf("%s-%s-01", parts[[2L]], parts[[3L]]))
    return(list(date = date, period = paste(parts[[2L]], parts[[3L]], sep = "-")))
  }
  if (frequency == "Q") {
    match <- regexec("^(\\d{4})0([1-4])SS$", code)
    parts <- regmatches(code, match)[[1L]]
    if (!length(parts)) {
      .nbs_abort(
        "nbs_period_schema_error",
        sprintf("Unexpected quarterly NBS period code `%s`.", code),
        period_code = code
      )
    }
    quarter <- as.integer(parts[[3L]])
    date <- as.Date(sprintf(
      "%s-%02d-01",
      parts[[2L]],
      (quarter - 1L) * 3L + 1L
    ))
    return(list(
      date = date,
      period = sprintf("%s-Q%d", parts[[2L]], quarter)
    ))
  }
  match <- regexec("^(\\d{4})YY$", code)
  parts <- regmatches(code, match)[[1L]]
  if (!length(parts)) {
    .nbs_abort(
      "nbs_period_schema_error",
      sprintf("Unexpected annual NBS period code `%s`.", code),
      period_code = code
    )
  }
  list(
    date = as.Date(sprintf("%s-01-01", parts[[2L]])),
    period = parts[[2L]]
  )
}
