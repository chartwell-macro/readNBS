test_that("monthly regularisation is explicit and uses working-day shares", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202608MM" =
      "cpi-missing-values.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )
  raw <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-01", "2026-08",
    client = client
  )
  raw <- raw[raw$period %in% c("2026-02", "2026-03", "2026-04"), ]
  raw$series_id <- "NBS/CN/M/RETAIL/TOTAL/YTD_LEVEL"
  raw$aggregation_basis <- "year_to_date"
  raw$period_policy <- "ytd_january_not_reported_february_combined"
  raw$value <- c(100, 160, 230)
  raw$is_missing <- FALSE

  result <- nbs_regularise_monthly(
    raw,
    method = "working_day_share",
    working_days = c("2026-01" = 22, "2026-02" = 18)
  )

  expect_equal(result$period, c("2026-01", "2026-02", "2026-03", "2026-04"))
  expect_equal(result$value, c(55, 45, 60, 70))
  expect_equal(unique(result$derived_method), "working_day_share")
  expect_equal(unique(result$aggregation_basis), "single_period")
  expect_equal(unique(result$span_months), 1L)
  expect_equal(
    result$source_observation_periods,
    c("2026-02", "2026-02", "2026-02,2026-03", "2026-03,2026-04")
  )
  expect_true(all(grepl("/DERIVED_MONTHLY_FLOW$", result$series_id)))
})

test_that("the transparent equal split is the labelled default and aliases are stable", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202608MM" =
      "cpi-missing-values.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )
  raw <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-01", "2026-08",
    client = client
  )
  raw <- raw[raw$period %in% c("2026-02", "2026-03"), ]
  raw$series_id <- "NBS/CN/M/RETAIL/TOTAL/YTD_LEVEL"
  raw$aggregation_basis <- "year_to_date"
  raw$value <- c(100, 160)

  benchmark <- nbs_regularise_monthly(raw)
  expect_equal(benchmark$value, c(50, 50, 60))
  expect_equal(unique(benchmark$derived_method), "equal_split_benchmark")
  expect_equal(
    nbs_regularise_monthly(raw, "equal_split_legacy")$value,
    benchmark$value
  )
  expect_equal(
    unique(nbs_regularise_monthly(raw, "equal_split_legacy")$derived_method),
    "equal_split_benchmark"
  )
  expect_equal(
    unique(nbs_regularise_monthly(raw, "equal_split_rba")$derived_method),
    "equal_split_benchmark"
  )

  partial_boundary <- raw[rep(1L, 3L), , drop = FALSE]
  partial_boundary$date <- as.Date(c(
    "2025-10-01", "2025-11-01", "2025-12-01"
  ))
  partial_boundary$period <- format(partial_boundary$date, "%Y-%m")
  partial_boundary$value <- c(10, 30, 60)
  partial_boundary$raw_sha256 <- paste0("partial-", seq_len(3L))
  boundary_input <- rbind(partial_boundary, raw)

  boundary <- nbs_regularise_monthly(boundary_input)
  expect_equal(boundary$period, c("2026-01", "2026-02", "2026-03"))
  expect_equal(boundary$value, c(50, 50, 60))
  expect_true(all(
    boundary$source_omitted_observation_periods ==
      "2025-10,2025-11,2025-12"
  ))

  boundary_working_days <- nbs_regularise_monthly(
    boundary_input,
    method = "working_day_share",
    working_days = c("2026-01" = 22, "2026-02" = 18)
  )
  expect_equal(boundary_working_days$value, c(55, 45, 60))

  percentage <- raw
  percentage$series_id <- "NBS/CN/M/RETAIL/TOTAL/YTD_YOY_PERCENT"
  expect_error(
    nbs_regularise_monthly(percentage),
    class = "nbs_regularisation_error"
  )
  incomplete <- raw[-1L, ]
  expect_error(
    nbs_regularise_monthly(incomplete),
    class = "nbs_regularisation_error"
  )
})

test_that("same-month comparison indexes convert exactly to percentage changes", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202608MM" =
      "cpi-missing-values.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )
  raw <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-01", "2026-08",
    client = client
  )
  raw <- raw[raw$period %in% c("2026-01", "2026-02", "2026-03"), ]
  raw$value <- c(105, 100.1, 98.2)
  raw$raw_value <- as.character(raw$value)
  raw_hashes <- raw$raw_sha256

  result <- nbs_comparison_index_to_percent(raw)

  expect_equal(result$value, c(5, 0.1, -1.8), tolerance = 1e-12)
  expect_equal(result$source_index_value, c(105, 100.1, 98.2))
  expect_equal(result$raw_value, c("105", "100.1", "98.2"))
  expect_equal(result$raw_sha256, raw_hashes)
  expect_equal(result$source_raw_sha256, raw_hashes)
  expect_equal(result$source_observation_periods, result$period)
  expect_true(all(result$unit == "Percent"))
  expect_true(all(result$unit_en == "Percent"))
  expect_true(all(result$unit_zh == "%"))
  expect_true(all(
    result$derived_method == "comparison_index_minus_100"
  ))
  expect_match(unique(result$title), "percentage change", fixed = TRUE)
  expect_equal(raw$value, c(105, 100.1, 98.2))
})

test_that("comparison-index conversion rejects other published measures", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202608MM" =
      "cpi-missing-values.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )
  raw <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-01", "2026-08",
    client = client
  )

  wrong_id <- raw
  wrong_id$series_id <- "NBS/CN/M/CPI/HEADLINE/MOM_INDEX"
  expect_error(
    nbs_comparison_index_to_percent(wrong_id),
    class = "nbs_regularisation_error"
  )

  wrong_unit <- raw
  wrong_unit$unit <- "Index (preceding month=100)"
  wrong_unit$unit_en <- wrong_unit$unit
  expect_error(
    nbs_comparison_index_to_percent(wrong_unit),
    class = "nbs_regularisation_error"
  )
})
