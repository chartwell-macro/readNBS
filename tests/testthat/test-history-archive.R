test_that("the normalized press archive matches every indexed release", {
  index <- readNBS:::.nbs_load_release_index()
  observations <- readNBS:::.nbs_bind_archive_rows(lapply(
    unique(index$dataset_id),
    function(dataset_id) {
      rows <- readNBS:::.nbs_press_observations_for_dataset(dataset_id)
      rows[rows$dataset_id == dataset_id, , drop = FALSE]
    }
  ))

  expect_true(nrow(index) > 11L)
  expect_equal(anyDuplicated(paste(index$dataset_id, index$period)), 0L)
  expect_setequal(unique(index$storage), "snapshot")
  expect_true(all(nzchar(index$schema_version)))
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
  source_pairs <- unique(index[c("source_url", "source_capture_sha256")])
  expect_equal(anyDuplicated(paste(
    source_pairs$source_url, source_pairs$source_capture_sha256, sep = "\r"
  )), 0L)

  actual <- vapply(seq_len(nrow(index)), function(row) {
    sum(
      observations$dataset_id == index$dataset_id[[row]] &
        observations$release_period == index$period[[row]]
    )
  }, integer(1))
  expect_equal(actual, index$expected_press_cells)

  for (dataset_id in unique(observations$dataset_id)) {
    manifest <- readNBS:::.nbs_load_dataset_manifest()
    required <- strsplit(
      manifest$required_dimensions[manifest$dataset_id == dataset_id],
      "|", fixed = TRUE
    )[[1L]]
    rows <- observations[observations$dataset_id == dataset_id, , drop = FALSE]
    columns <- sub("^category_code$", "category", required)
    filters <- rows[, columns, drop = FALSE]
    names(filters) <- required
    keys <- vapply(seq_len(nrow(filters)), function(row) {
      nbs_dimension_key(as.list(filters[row, , drop = FALSE]))
    }, character(1))
    expect_true(
      all(keys %in% nbs_dataset_valid_tuples(dataset_id)$dimension_key),
      info = dataset_id
    )
  }
})

test_that("archive rows carry the indexed schema and basket versions", {
  releases <- readNBS:::.nbs_load_release_index()
  observations <- readNBS:::.nbs_bind_archive_rows(lapply(
    unique(releases$dataset_id),
    function(dataset_id) {
      rows <- readNBS:::.nbs_press_observations_for_dataset(dataset_id)
      rows[rows$dataset_id == dataset_id, , drop = FALSE]
    }
  ))
  observation_key <- paste(
    observations$dataset_id,
    observations$release_period,
    sep = "\r"
  )
  release_key <- paste(
    releases$dataset_id, releases$period, sep = "\r"
  )
  matched <- match(observation_key, release_key)

  expect_false(anyNA(matched))
  schema_matches <- observations$schema_version == releases$schema_version[matched]
  if ("classification_id" %in% names(observations)) {
    schema_matches <- schema_matches |
      observations$classification_id == releases$schema_version[matched]
  }
  expect_true(all(schema_matches))
  has_basket <- nzchar(observations$basket_version)
  expect_identical(
    observations$basket_version[has_basket],
    releases$basket_version[matched][has_basket]
  )
})

test_that("exact retail and industrial headline leaves use portal history", {
  retail <- nbs_resolve_dataset_series(
    "NBS/CN/M/RETAIL_SALES/DETAIL",
    list(
      category_code = "total_retail_sales_of_consumer_goods",
      measure = "level",
      period_basis = "ytd"
    )
  )
  industrial <- nbs_resolve_dataset_series(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    list(
      category = "total",
      measure = "total_profit_yoy",
      period_basis = "ytd"
    )
  )

  expect_equal(retail$route, "portal_uuid_series")
  expect_equal(industrial$route, "portal_uuid_series")
  expect_lte(retail$available_from, as.Date("2018-02-01"))
  expect_lte(industrial$available_from, as.Date("2018-02-01"))

  broad <- nbs_dataset_catalogue()
  broad_retail <- broad$available_from[
    broad$dataset_id == "NBS/CN/M/RETAIL_SALES/DETAIL"
  ]
  broad_industrial <- broad$available_from[
    broad$dataset_id ==
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE"
  ]
  expect_equal(broad_retail, as.Date("1990-01-01"))
  expect_equal(broad_industrial, as.Date("1999-01-01"))

  expected_mapped <- c(
    "NBS/CN/M/RETAIL_SALES/DETAIL" = 99L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" = 6L
  )
  for (dataset_id in names(expected_mapped)) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    dimensions <- setdiff(names(tuples), "dimension_key")
    mapped <- vapply(seq_len(nrow(tuples)), function(row) {
      readNBS:::.nbs_dataset_portal_series_id(
        dataset_id,
        as.list(tuples[row, dimensions, drop = FALSE])
      )
    }, character(1))
    mapped <- mapped[nzchar(mapped)]
    expect_length(mapped, expected_mapped[[dataset_id]])
    expect_equal(anyDuplicated(mapped), 0L, info = dataset_id)
    expect_true(all(mapped %in% nbs_catalogue()$series_id), info = dataset_id)
  }
})

test_that("data-driven portal map is internally consistent", {
  map <- readNBS:::.nbs_load_dataset_portal_map()
  exact <- map[map$route_type == "exact", , drop = FALSE]
  expect_equal(
    anyDuplicated(paste(exact$dataset_id, exact$dimension_key)),
    0L
  )
  expect_true(all(exact$series_id %in% nbs_catalogue()$series_id))

  retail <- map[map$route_type == "retail_category", , drop = FALSE]
  expect_equal(
    anyDuplicated(paste(retail$dataset_id, retail$category_code)),
    0L
  )
  expect_true(all(grepl("^NBS/CN/M/RETAIL/", retail$series_id)))
})

test_that("unmapped exact leaves retain their snapshot route", {
  filters <- list(
    category_code =
      "of_which_retail_sales_of_consumer_goods_excluding_automobiles",
    measure = "level",
    period_basis = "ytd"
  )
  expect_identical(
    readNBS:::.nbs_dataset_portal_series_id(
      "NBS/CN/M/RETAIL_SALES/DETAIL", filters
    ),
    ""
  )
  client <- nbs_client(
    transport = function(request) stop("snapshot fetch must not use transport")
  )
  result <- nbs_fetch_dataset(
    "NBS/CN/M/RETAIL_SALES/DETAIL",
    filters = filters,
    start = "2025-08",
    end = "2025-08",
    client = client
  )
  expect_setequal(unique(result$data_status), "first_release_snapshot")
})

test_that("archived exact tuples fetch without a document request", {
  index <- readNBS:::.nbs_load_release_index()
  housing <- index[index$dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY", ]
  skip_if(!nrow(housing))
  base <- if (any(housing$index_base == "2020")) "2020" else "2025"
  bounds <- range(as.Date(housing$reference_period_end[housing$index_base == base]))
  calls <- 0L
  client <- nbs_client(
    transport = function(request) {
      calls <<- calls + 1L
      stop("snapshot fetch must not use the transport")
    }
  )
  result <- nbs_fetch_dataset(
    "NBS/CN/M/HOUSING_PRICES/70_CITY",
    filters = list(
      city_code = "110000",
      market = "new",
      size_band = "all",
      comparison = "mom",
      index_base = base
    ),
    start = bounds[[1L]],
    end = bounds[[2L]],
    client = client
  )

  expect_gt(nrow(result), 0L)
  expect_equal(calls, 0L)
  expect_setequal(unique(result$data_status), "first_release_snapshot")
})

test_that("housing comparison bases retain exact and continuous choices", {
  dimensions <- nbs_dataset_dimensions(
    "NBS/CN/M/HOUSING_PRICES/70_CITY", "en"
  )
  bases <- dimensions[
    dimensions$dimension == "index_base",
    c("value", "valid_from", "valid_to")
  ]
  expect_setequal(
    bases$value,
    c("all_official_bases", "2010", "2015", "2020", "2025")
  )
  expect_equal(
    bases$valid_from[bases$value == "all_official_bases"], "2011-01-01"
  )
  expect_equal(bases$valid_from[bases$value == "2010"], "2011-01-01")
  expect_equal(bases$valid_to[bases$value == "2010"], "2015-12-31")
  expect_equal(bases$valid_to[bases$value == "2020"], "2025-12-31")
  expect_equal(bases$valid_from[bases$value == "2025"], "2026-01-01")
})

test_that("housing monthly change defaults to full official history", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_identical(tuples$index_base[[1L]], "all_official_bases")
  expect_identical(tuples$comparison[[1L]], "mom")

  filters <- as.list(tuples[1L, setdiff(names(tuples), "dimension_key")])
  expect_identical(filters$city_code, "110000")
  expect_identical(filters$market, "new")
  expect_identical(filters$size_band, "all")

  calls <- 0L
  client <- nbs_client(transport = function(request) {
    calls <<- calls + 1L
    stop("snapshot fetch must not use the transport")
  })
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
  result <- nbs_fetch_dataset(dataset_id, filters, client = client)

  expect_equal(calls, 0L)
  expect_identical(descriptor$available_from, as.Date("2011-01-01"))
  expect_identical(descriptor$available_to, as.Date("2026-06-30"))
  expect_equal(nrow(result), 186L)
  expect_identical(min(result$date), as.Date("2011-01-01"))
  expect_identical(max(result$date), as.Date("2026-06-01"))
  expect_identical(anyDuplicated(result$date), 0L)
  expect_setequal(unique(result$index_base), c("2010", "2015", "2020", "2025"))
  expect_identical(unique(result$dimension_key), nbs_dimension_key(filters))
  expect_true(all(grepl("^https://www\\.stats\\.gov\\.cn/", result$source_url)))

  expected_dates <- seq(as.Date("2011-01-01"), as.Date("2026-06-01"), by = "month")
  expect_setequal(result$date, expected_dates)
})

test_that("housing YTD-average change follows the full official-base history", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  filters <- list(
    city_code = "110000", market = "new", size_band = "all",
    comparison = "ytd_avg_yoy", index_base = "all_official_bases"
  )

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_true(nbs_dimension_key(filters) %in% tuples$dimension_key)

  calls <- 0L
  client <- nbs_client(transport = function(request) {
    calls <<- calls + 1L
    stop("snapshot fetch must not use the transport")
  })
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
  result <- nbs_fetch_dataset(dataset_id, filters, client = client)

  expect_equal(calls, 0L)
  expect_identical(descriptor$available_from, as.Date("2023-01-01"))
  expect_identical(descriptor$available_to, as.Date("2026-06-30"))
  expect_equal(nrow(result), 39L)
  expect_identical(min(result$date), as.Date("2023-01-01"))
  expect_identical(max(result$date), as.Date("2026-06-01"))
  expect_identical(anyDuplicated(result$date), 0L)
  expect_setequal(unique(result$index_base), c("2020", "2025"))
  expect_identical(unique(result$dimension_key), nbs_dimension_key(filters))
})

test_that("housing fixed-base levels cannot use the continuous base choice", {
  expect_error(
    nbs_resolve_dataset_series(
      "NBS/CN/M/HOUSING_PRICES/70_CITY",
      list(
        city_code = "110000", market = "new", size_band = "all",
        comparison = "fixed_base", index_base = "all_official_bases"
      )
    ),
    class = "nbs_dataset_filter_error"
  )
})

test_that("housing fixed-base history begins with the January 2011 release", {
  index <- readNBS:::.nbs_load_release_index()
  history <- index[
    index$dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY" &
      index$index_base == "2010",
    , drop = FALSE
  ]
  skip_if(!nrow(history))
  expect_identical(min(history$period), "2011-01")
  expect_true(all(grepl("^https://www\\.stats\\.gov\\.cn/", history$source_url)))

  calls <- 0L
  client <- nbs_client(transport = function(request) {
    calls <<- calls + 1L
    stop("snapshot fetch must not use the transport")
  })
  result <- nbs_fetch_dataset(
    "NBS/CN/M/HOUSING_PRICES/70_CITY",
    filters = list(
      city_code = "110000", market = "new", size_band = "all",
      comparison = "mom", index_base = "2010"
    ),
    start = "2011-01", end = "2011-01", client = client
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, 101.0)
  expect_equal(calls, 0L)
})

test_that("monthly bounds select reported periods, not later YTD windows", {
  dataset_id <- "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE"
  filters <- list(
    category = "private",
    measure = "total_profit_level",
    period_basis = "ytd"
  )
  index <- readNBS:::.nbs_load_release_index()
  releases <- index[index$dataset_id == dataset_id, , drop = FALSE]
  skip_if(!any(releases$period == "2024-05"))

  may <- nbs_fetch_dataset(
    dataset_id, filters, start = "2024-05", end = "2024-05"
  )
  expect_setequal(unique(may$period), "2024-05")
  expect_error(
    nbs_fetch_dataset(
      dataset_id, filters, start = "2026-01", end = "2026-01"
    ),
    class = "nbs_range_error"
  )
})

test_that("capacity exact leaves reach pre-portal first releases", {
  dataset_id <- "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION"
  series_id <- "NBS/CN/Q/CAPACITY_UTILISATION/TOTAL/CURRENT_PERCENT"
  client <- nbs_client(
    transport = function(request) stop("pre-portal fetch must stay local")
  )

  descriptor <- nbs_resolve_dataset_series(
    dataset_id, list(series_id = series_id)
  )
  result <- nbs_fetch_dataset(
    dataset_id,
    filters = list(series_id = series_id),
    start = "2018-Q2",
    end = "2020-Q4",
    client = client
  )

  expect_equal(descriptor$available_from, as.Date("2018-04-01"))
  expect_true(is.na(descriptor$available_to))
  expect_equal(descriptor$aggregation_basis, "single_period")
  expect_gt(nrow(result), 0L)
  expect_equal(min(result$date), as.Date("2018-04-01"))
  expect_true(all(result$aggregation_basis == "single_period"))
  expect_true(all(result$data_status == "first_release_snapshot"))
  expect_true(all(result$route == "press_snapshot"))
  expect_true(all(result$source_vintage == "first_release_press_archive"))
})

test_that("capacity hybrid results preserve row-level route provenance", {
  dataset_id <- "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION"
  series_id <- "NBS/CN/Q/CAPACITY_UTILISATION/TOTAL/CURRENT_PERCENT"
  scalar <- nbs_manifest()
  scalar <- scalar[scalar$series_id == series_id, , drop = FALSE]
  client <- nbs_client(
    retries = 0L,
    transport = function(request) {
      payload <- list(
        data = list(list(
          code = "202101SS",
          name = "2021-Q1",
          values = list(list(
            `_id` = scalar$indicator_uuid[[1L]],
            value = "77.2",
            du_name = "%",
            da = scalar$geography_code[[1L]],
            da_name = scalar$geography_en[[1L]],
            catalogid = scalar$catalog_uuid[[1L]]
          ))
        )),
        success = TRUE,
        state = 20000,
        message = "success"
      )
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE)
      )
    }
  )
  result <- nbs_fetch_dataset(
    dataset_id,
    list(series_id = series_id),
    start = "2018-Q2",
    end = "2021-Q1",
    client = client
  )

  expect_setequal(
    unique(result$route),
    c("press_snapshot", "portal_uuid_series")
  )
  expect_setequal(
    unique(result$source_vintage),
    c("first_release_press_archive", "latest_revised_database")
  )
  expect_true(all(
    result$route[result$date < as.Date("2021-01-01")] == "press_snapshot"
  ))
  expect_identical(
    result$route[result$date == as.Date("2021-01-01")],
    "portal_uuid_series"
  )
})

test_that("wage bounds select the published observation year", {
  dataset_id <- "NBS/CN/A/EMPLOYMENT_WAGES"
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  common <- tuples$unit_type == "non_private" &
    tuples$region == "not_applicable" &
    tuples$industry == "total" &
    tuples$registration == "not_applicable" &
    tuples$occupation == "not_applicable"
  prior <- tuples[
    common & tuples$measure == "prior_year_average_wage", , drop = FALSE
  ]
  current <- tuples[
    common & tuples$measure == "current_year_average_wage", , drop = FALSE
  ]
  filter_names <- setdiff(names(tuples), "dimension_key")

  prior_result <- nbs_fetch_dataset(
    dataset_id,
    filters = as.list(prior[1L, filter_names, drop = FALSE]),
    start = "2022",
    end = "2022"
  )
  current_result <- nbs_fetch_dataset(
    dataset_id,
    filters = as.list(current[1L, filter_names, drop = FALSE]),
    start = "2023",
    end = "2023"
  )

  expect_setequal(unique(prior_result$period), "2022")
  expect_setequal(unique(current_result$period), "2023")
  expect_true(all(as.Date(prior_result$date) == as.Date("2022-01-01")))
  expect_true(all(as.Date(current_result$date) == as.Date("2023-01-01")))
})
