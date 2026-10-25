test_that("Yearbook discovery resolves official routes rather than filenames", {
  index <- fixture_body("yearbook-2011-left.html")
  discovered <- nbs_yearbook_discover(index, 2011L)

  expect_equal(nrow(discovered), 6L)
  expect_true(all(grepl("^https://www\\.stats\\.gov\\.cn/sj/ndsj/2011/html/", discovered$source_url)))
  expect_equal(discovered$source_table, c("3-1", "4-15", "5-27", "10-1", "14-1", "17-22"))

  routes <- nbs_yearbook_routes(index, 2011L)
  expect_setequal(routes$family, c(
    "demography", "wages", "property", "households", "industrial_benefits", "retail"
  ))
  expect_false(any(grepl("[A-Z][0-9]{4}[A-Z]", nbs_yearbook_targets(2011L)$source_table)))
  expect_true(all(nzchar(routes$schema_era)))
})

test_that("Yearbook parser expands merged headers and preserves provenance", {
  routes <- nbs_yearbook_routes(fixture_body("yearbook-2011-left.html"), 2011L)
  route <- routes[routes$source_table == "3-1", , drop = FALSE]
  route$expected_columns <- 3L
  route$expected_min_rows <- 5L
  route$header_rows <- 3L
  result <- nbs_yearbook_parse_table(
    fixture_body("yearbook-table-merged.html"), route,
    retrieved_at = as.POSIXct("2026-08-11 00:00:00", tz = "UTC")
  )

  expect_equal(nrow(result), 4L)
  expect_equal(result$reference_year, c(2009L, 2009L, 2010L, 2010L))
  expect_equal(result$measure, c("人口 / 总人口", "人口 / 城镇", "人口 / 总人口", "人口 / 城镇"))
  expect_true(all(result$source_yearbook == 2011L))
  expect_true(all(result$source_table == "3-1"))
  expect_true(all(grepl('"item":"20', result$dimensions, fixed = TRUE)))
  expect_true(all(grepl("^[0-9a-f]{64}$", result$raw_sha256)))
  expect_identical(unique(result$schema_era), "annual_population_census_revised")
})

test_that("Yearbook parser fails closed on changed geometry", {
  routes <- nbs_yearbook_routes(fixture_body("yearbook-2011-left.html"), 2011L)
  route <- routes[routes$source_table == "3-1", , drop = FALSE]
  route$expected_columns <- 3L
  route$expected_min_rows <- 2L
  route$header_rows <- 1L

  expect_error(
    nbs_yearbook_parse_table(fixture_body("yearbook-table-geometry-break.html"), route),
    class = "nbs_yearbook_schema_error"
  )
})

test_that("live Yearbook discovery is explicit and fixture-backed", {
  client <- nbs_client(
    transport = function(request) {
      expect_identical(request$method, "GET")
      expect_identical(request$url, "https://www.stats.gov.cn/sj/ndsj/2011/left.htm")
      fixture_response("yearbook-2011-left.html", content_type = "text/html")
    },
    retries = 0L,
    now = fixed_now
  )
  result <- nbs_yearbook_discover_live(2011L, client)
  expect_equal(nrow(result), 6L)
  expect_s3_class(attr(result, "retrieved_at"), "POSIXct")
  expect_match(attr(result, "raw_sha256"), "^[0-9a-f]{64}$")
})

test_that("frozen 2011 Yearbook snapshots preserve bytes, decode and cardinality", {
  snapshots <- readNBS:::.nbs_load_yearbook_snapshot_index()
  targets <- nbs_yearbook_targets(2011L)
  routes <- merge(
    snapshots, targets,
    by = c("source_yearbook", "source_table", "family", "schema_era"),
    all.x = TRUE, sort = FALSE
  )
  expected_2010 <- c(
    demography = 9L, wages = 80L, property = 36L, households = 51L,
    industrial_benefits = 210L, retail = 64L
  )
  actual <- vapply(seq_len(nrow(routes)), function(index) {
    raw <- readNBS:::.nbs_yearbook_snapshot_raw(routes[index, , drop = FALSE])
    html <- readNBS:::.nbs_yearbook_decode_html(raw, routes$encoding[[index]])
    route <- routes[index, c(
      "source_yearbook", "family", "source_table", "source_url", "schema_era",
      "expected_columns", "expected_min_rows", "header_rows"
    ), drop = FALSE]
    rows <- nbs_yearbook_parse_table(
      html, route,
      retrieved_at = as.POSIXct(routes$retrieved_at[[index]], tz = "UTC"),
      raw_sha256 = routes$raw_sha256[[index]]
    )
    sum(rows$reference_year == 2010L)
  }, integer(1L))
  names(actual) <- routes$family
  expect_identical(actual[names(expected_2010)], expected_2010)
})

test_that("Yearbook archive exposes bounded 2010 annual legacy tuples offline", {
  releases <- readNBS:::.nbs_load_yearbook_release_index()
  archive <- readNBS:::.nbs_load_yearbook_observations()
  expect_equal(nrow(releases), 6L)
  expect_equal(nrow(archive), 450L)
  expect_true(all(releases$period == "2010"))
  observed <- vapply(seq_len(nrow(releases)), function(index) {
    sum(
      archive$dataset_id == releases$dataset_id[[index]] &
        archive$release_period == releases$period[[index]]
    )
  }, integer(1L))
  expect_identical(observed, releases$expected_yearbook_cells)
})

test_that("Yearbook bridge metadata carries source labels and published units", {
  map <- readNBS:::.nbs_load_yearbook_label_map()
  archive <- readNBS:::.nbs_load_yearbook_observations()
  dimensions <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-yearbook-dataset-dimensions.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
  )
  placeholders <- c("Official Yearbook", "Official published unit")
  expect_false(any(vapply(placeholders, function(value) any(grepl(
    value, c(map$dimension_label_en, map$unit_en), fixed = TRUE
  )), logical(1L))))
  expect_false(any(vapply(placeholders, function(value) any(grepl(
    value, c(dimensions$label_en, dimensions$unit_en,
      archive$dimension_label_en, archive$unit_en), fixed = TRUE
  )), logical(1L))))
  expect_setequal(unique(map$unit_en), c(
    "10,000 persons", "10,000 square metres", "100 million yuan", "Beds",
    "Enterprises", "Handsets per 100 persons", "Percent", "Persons",
    "Seats", "Sets per 100 households", "Square metres", "Standard vehicles",
    "Vehicles", "Yuan", "Yuan per square metre"
  ))
})

test_that("Yearbook defaults use the canonical annual date window", {
  default_series <- c(
    "NBS/CN/A/YEARBOOK_DEMOGRAPHY_LEGACY" = "yb_1d79d069bad3ce75",
    "NBS/CN/A/YEARBOOK_WAGES_URBAN_UNITS_LEGACY" =
      "yb_02376aade7c23273",
    "NBS/CN/A/YEARBOOK_PROPERTY_DEVELOPMENT_LEGACY" =
      "yb_0b0687e99afcdfbf",
    "NBS/CN/A/YEARBOOK_HOUSEHOLDS_URBAN_RURAL_LEGACY" =
      "yb_0882d1e7179e8f76",
    "NBS/CN/A/YEARBOOK_INDUSTRIAL_BENEFITS_LEGACY" =
      "yb_01914f2c7c14927e",
    "NBS/CN/A/YEARBOOK_RETAIL_REGIONAL_LEGACY" =
      "yb_075f54a0a5116ca3"
  )
  client <- nbs_client(
    transport = function(request) stop("Yearbook snapshot fetch used network")
  )
  catalogue <- nbs_dataset_catalogue()
  required_provenance <-
    readNBS:::.nbs_dataset_observation_provenance_fields
  expect_setequal(names(default_series), intersect(
    names(default_series), catalogue$dataset_id
  ))
  for (dataset_id in names(default_series)) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    expect_equal(ncol(tuples), 2L)
    expect_identical(tuples$series[[1L]], default_series[[dataset_id]])
    filters <- list(series = default_series[[dataset_id]])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
    expect_identical(descriptor$available_from, as.Date("2010-01-01"))
    expect_identical(descriptor$available_to, as.Date("2010-12-31"))
    result <- nbs_fetch_dataset(
      dataset_id,
      filters = filters,
      start = as.Date("2010-01-01"),
      end = as.Date("2010-01-01"),
      client = client
    )
    expect_equal(nrow(result), 1L)
    expect_identical(result$date, as.Date("2010-01-01"), info = dataset_id)
    expect_identical(
      result$reference_date, as.Date("2010-01-01"), info = dataset_id
    )
    expect_identical(
      result$reference_period_start, as.Date("2010-01-01"), info = dataset_id
    )
    expect_identical(
      result$reference_period_end, as.Date("2010-12-31"), info = dataset_id
    )
    expect_true(
      all(required_provenance %in% names(result)),
      info = dataset_id
    )
    expect_false(any(vapply(
      result[required_provenance],
      function(value) anyNA(value) || any(!nzchar(trimws(as.character(value)))),
      logical(1L)
    )), info = dataset_id)
    expect_identical(result$unit, result$unit_en, info = dataset_id)
    expect_identical(result$schema_era[[1L]],
      readNBS:::.nbs_load_yearbook_release_index()$schema_era[
        readNBS:::.nbs_load_yearbook_release_index()$dataset_id == dataset_id
      ][[1L]])
    expect_match(result$raw_sha256[[1L]], "^[0-9a-f]{64}$")
  }
})

test_that("Yearbook demography exposes the canonical localized unit", {
  dataset_id <- "NBS/CN/A/YEARBOOK_DEMOGRAPHY_LEGACY"
  filters <- list(series = "yb_1d79d069bad3ce75")
  client <- nbs_client(
    transport = function(request) stop("Yearbook snapshot fetch used network")
  )
  english <- nbs_fetch_dataset(
    dataset_id, filters, "2010", "2010", "en", client
  )
  chinese <- nbs_fetch_dataset(
    dataset_id, filters, "2010", "2010", "zh", client
  )

  expect_identical(english$unit[[1L]], "10,000 persons")
  expect_identical(english$unit, english$unit_en)
  expect_identical(chinese$unit[[1L]], "万人")
  expect_identical(chinese$unit, chinese$unit_zh)
})

test_that("Yearbook tuples use the Yearbook archive in the coverage audit", {
  ids <- nbs_dataset_catalogue()$dataset_id
  ids <- ids[grepl("/YEARBOOK_", ids, fixed = TRUE)]
  expect_length(ids, 6L)
  for (dataset_id in ids) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    coverage <- readNBS:::.nbs_dataset_snapshot_coverages(dataset_id, tuples)
    expect_true(all(coverage$available_from == as.Date("2010-01-01")))
    expect_true(all(coverage$available_to == as.Date("2010-12-31")))
    expect_true(all(nzchar(coverage$schema_era)))
    expect_false(any(coverage$schema_era == "manifest_default"))
  }
})
