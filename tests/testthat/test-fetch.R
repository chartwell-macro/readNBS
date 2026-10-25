test_that("CPI stitches Dec 2025 and Jan 2026 with explicit identities", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$requests <- list()
  mapping <- list(
    "809d2522b0fe4be89142650341b19083::202512MM-202512MM" =
      "cpi-2025-december.json",
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202601MM" =
      "cpi-2026-january.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping, calls),
    retries = 0L,
    now = fixed_now
  )

  result <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    start = "2025-12",
    end = "2026-01",
    client = client
  )

  expect_equal(calls$n, 2L)
  expect_equal(result$period, c("2025-12", "2026-01"))
  expect_equal(result$value, c(100.8, 100.2))
  expect_equal(
    result$classification_id,
    c("cpi-2021-2025", "cpi-2026-")
  )
  expect_equal(anyDuplicated(result$period), 0L)
  expect_equal(unique(result$stitch_policy), "explicit_headline_continuity")
  expect_match(unique(result$series_note), "January 2026")
  expect_equal(
    unique(result$unit),
    "Index (same month previous year=100)"
  )
  expect_equal(unique(result$raw_unit), "%")
  expect_true(all(nchar(result$raw_sha256) == 64L))
  expect_equal(unique(result$parser_version), "2")
  expect_equal(
    unique(result$package_version),
    as.character(utils::packageVersion("readNBS"))
  )
  expect_equal(result$reference_period, result$period)
  expect_true(all(is.na(result$release_date)))
  expect_true(all(is.na(result$vintage_date)))
  expect_equal(unique(result$data_status), "latest_revised")
})

test_that("the original sentinels preserve exact native values and units", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202606MM-202606MM" =
      "cpi-2026-june.json",
    "28d936104e304aa191e338eb82b6dc09::202602SS-202602SS" =
      "gdp-2026-q2.json",
    "6331ad868e8b4f55b8e9b6e765609ce1::2025YY-2025YY" =
      "population-2025.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )

  cpi <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  gdp <- nbs_fetch_series(
    "NBS/CN/Q/GDP/NOMINAL/CURRENT_QUARTER",
    "2026-Q2", "2026-Q2",
    client = client
  )
  population <- nbs_fetch_series(
    "NBS/CN/A/POPULATION/TOTAL/YEAR_END",
    "2025", "2025",
    client = client
  )

  expect_equal(cpi$value, 101.0)
  expect_equal(cpi$unit, "Index (same month previous year=100)")
  expect_equal(gdp$value, 361511.1)
  expect_equal(gdp$unit, "100 million yuan")
  expect_equal(gdp$date, as.Date("2026-04-01"))
  expect_equal(population$value, 140489)
  expect_equal(population$unit, "10,000 persons")
  expect_equal(population$date, as.Date("2025-01-01"))
})

test_that("known missing tokens are NA while zero remains zero", {
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202601MM-202608MM" =
      "cpi-missing-values.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping),
    retries = 0L,
    now = fixed_now
  )
  result <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-01", "2026-08",
    client = client
  )

  expect_true(all(is.na(result$value[1:6])))
  expect_true(all(result$is_missing[1:6]))
  expect_identical(result$value[[7L]], 0)
  expect_false(result$is_missing[[7L]])
  expect_equal(result$value[[8L]], 1234.5)
  expect_error(
    readNBS:::.nbs_parse_value("suppressed?"),
    class = "nbs_value_error"
  )
})

test_that("omitted end uses the previous completed calendar period", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$requests <- list()
  mapping <- list(
    "5c7452825c7c4dcba391db5ca7f335c5::202606MM-202606MM" =
      "cpi-2026-june.json"
  )
  client <- nbs_client(
    transport = make_observation_transport(mapping, calls),
    retries = 0L,
    now = fixed_now
  )

  result <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    start = "2026-06",
    client = client
  )

  expect_equal(result$period, "2026-06")
  expect_equal(calls$requests[[1L]]$body$dts[[1L]], "202606MM-202606MM")
  expect_equal(
    readNBS:::.nbs_previous_completed_period(
      as.Date("2026-07-29"),
      "Q"
    ),
    as.Date("2026-04-01")
  )
  expect_equal(
    readNBS:::.nbs_previous_completed_period(
      as.Date("2026-07-29"),
      "A"
    ),
    as.Date("2025-01-01")
  )
})

test_that("policy-excluded January placeholders are not returned", {
  slice <- nbs_manifest()
  slice <- slice[
    slice$series_id == "NBS/CN/M/CPI/HEADLINE/YOY_INDEX" &
      slice$slice_start == "2026-01-01",
    ,
    drop = FALSE
  ]
  slice$period_policy <- "omit_january"
  client <- nbs_client(
    transport = function(request) {
      fixture_response("cpi-2026-january.json")
    },
    retries = 0L,
    now = fixed_now
  )

  result <- readNBS:::.nbs_fetch_slice(
    client,
    slice,
    as.Date("2026-01-01"),
    as.Date("2026-01-01"),
    "en"
  )
  expect_equal(nrow(result), 0L)
  expect_identical(names(result), names(readNBS:::.nbs_empty_observations()))
})

test_that("omitted response periods fail closed", {
  incomplete_client <- nbs_client(
    transport = function(request) {
      fixture_response("cpi-2026-january.json")
    },
    retries = 0L,
    now = fixed_now
  )

  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-01", "2026-02",
      client = incomplete_client
    ),
    class = "nbs_incomplete_period_error"
  )
})

test_that("observation schema, identity and overlap failures are explicit", {
  bad_schema <- nbs_client(
    transport = function(request) {
      fixture_response("schema-missing-values.json")
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-01", "2026-01",
      client = bad_schema
    ),
    class = "nbs_schema_error"
  )

  widened <- jsonlite::fromJSON(
    fixture_body("cpi-2026-january.json"),
    simplifyVector = FALSE
  )
  widened$data[[1L]]$values[[2L]] <- widened$data[[1L]]$values[[1L]]
  widened_client <- nbs_client(
    transport = function(request) {
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = jsonlite::toJSON(widened, auto_unbox = TRUE)
      )
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-01", "2026-01",
      client = widened_client
    ),
    class = "nbs_schema_error"
  )

  duplicate_period <- jsonlite::fromJSON(
    fixture_body("cpi-2026-january.json"),
    simplifyVector = FALSE
  )
  duplicate_period$data[[2L]] <- duplicate_period$data[[1L]]
  duplicate_client <- nbs_client(
    transport = function(request) {
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = jsonlite::toJSON(duplicate_period, auto_unbox = TRUE)
      )
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-01", "2026-01",
      client = duplicate_client
    ),
    class = "nbs_period_overlap_error"
  )
})

test_that("period count is bounded before transport", {
  calls <- 0L
  client <- nbs_client(
    max_periods = 2L,
    transport = function(request) {
      calls <<- calls + 1L
      stop("must not call")
    },
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-01", "2026-03",
      client = client
    ),
    class = "nbs_period_limit_error"
  )
  expect_equal(calls, 0L)
})
