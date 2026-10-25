test_that("chart-ready wage histories replace classification-era fragments", {
  dataset_id <- "NBS/CN/A/WAGES/LONGITUDINAL"
  catalogue <- nbs_dataset_catalogue()
  expect_equal(nrow(catalogue), 39L)
  expect_true(dataset_id %in% catalogue$dataset_id)
  expect_identical(
    catalogue$release_id[catalogue$dataset_id == dataset_id],
    "cn_nbs_wages"
  )

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  archive <- readNBS:::.nbs_load_longitudinal_observations()
  archive <- archive[archive$dataset_id == dataset_id, , drop = FALSE]
  expect_equal(nrow(tuples), 94L)
  expect_equal(nrow(archive), 1421L)
  expect_identical(
    as.list(tuples[1L, c("unit_type", "breakdown", "category", "measure")]),
    list(
      unit_type = "non_private", breakdown = "overall",
      category = "total", measure = "average_wage"
    )
  )

  tuple_key <- paste(
    archive$unit_type, archive$breakdown, archive$category,
    archive$measure, sep = "\r"
  )
  counts <- table(tuple_key)
  expect_gte(min(as.integer(counts)), 13L)
  expect_lte(max(as.integer(counts)), 16L)
  expect_equal(anyDuplicated(paste(
    tuple_key, archive$observation_date, sep = "\r"
  )), 0L)

  filters <- list(
    unit_type = "non_private", breakdown = "overall",
    category = "total", measure = "average_wage"
  )
  client <- nbs_client(transport = function(request) {
    stop("longitudinal wage history must not use the network")
  })
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
  result <- nbs_fetch_dataset(dataset_id, filters, client = client)
  expect_equal(nrow(result), 16L)
  expect_identical(result$date, as.Date(sprintf("%d-01-01", 2010:2025)))
  expect_equal(result$value[c(1L, 13L, 16L)], c(37147, 114029, 129441))
  expect_identical(descriptor$unit[[1L]], "Yuan")
  expect_identical(descriptor$route[[1L]], "retained_longitudinal_series")
  expect_match(descriptor$title[[1L]], "longitudinal series")
  expect_match(descriptor$series_note[[1L]], "2012")
  expect_true(all(result$data_status == "latest_retained_official_observation"))
  expect_true(all(result$route == "retained_longitudinal_series"))
  expect_true(all(grepl("^https://www[.]stats[.]gov[.]cn/", result$source_url)))
  expect_true(all(grepl("^[0-9a-f]{64}$", result$raw_sha256)))
})

test_that("sparse wage source tables remain auditable but are not the default series", {
  raw_id <- "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY"
  filters <- list(
    classification = "gbt4754_2011", table = "industry",
    region = "not_applicable",
    industry = "agriculture_forestry_animal_husbandry_and_fishery",
    registration = "not_applicable", measure = "average_wage"
  )
  raw <- nbs_fetch_dataset(
    raw_id, filters,
    client = nbs_client(transport = function(request) {
      stop("retained wage table must not use the network")
    })
  )
  expect_equal(nrow(raw), 6L)
  expect_identical(raw$date, as.Date(sprintf("%d-01-01", 2012:2017)))

  longitudinal <- readNBS:::.nbs_load_longitudinal_observations()
  longitudinal <- longitudinal[
    longitudinal$dataset_id == "NBS/CN/A/WAGES/LONGITUDINAL" &
      longitudinal$unit_type == "non_private" &
      longitudinal$breakdown == "industry" &
      longitudinal$category ==
        "agriculture_forestry_animal_husbandry_and_fishery" &
      longitudinal$measure == "average_wage",
    , drop = FALSE
  ]
  expect_equal(nrow(longitudinal), 16L)
  expect_identical(
    as.Date(longitudinal$observation_date),
    as.Date(sprintf("%d-01-01", 2010:2025))
  )
})

test_that("migrant-worker longitudinal selectors are substantive series", {
  dataset_id <- "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  catalogue <- nbs_dataset_catalogue()
  expect_identical(
    catalogue$release_id[catalogue$dataset_id == dataset_id],
    "cn_nbs_migrant_workers"
  )
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  dimensions <- nbs_dataset_dimensions(dataset_id)
  archive <- readNBS:::.nbs_load_longitudinal_observations()
  archive <- archive[archive$dataset_id == dataset_id, , drop = FALSE]

  expect_equal(nrow(tuples), 50L)
  expect_equal(nrow(archive), 647L)
  first_member <- dimensions[
    dimensions$dimension == "series" &
      dimensions$value == tuples$series[[1L]], , drop = FALSE
  ]
  expect_identical(
    first_member$label_en[[1L]],
    "Migrant-worker numbers — Migrant workers, total"
  )
  counts <- table(archive$series)
  expect_gte(min(as.integer(counts)), 9L)
  expect_equal(max(as.integer(counts)), 18L)
  expect_equal(anyDuplicated(paste(
    archive$series, archive$observation_date, sep = "\r"
  )), 0L)

  series <- dimensions$value[
    dimensions$dimension == "series" &
      dimensions$label_en ==
        "Migrant-worker numbers — Migrant workers, total"
  ]
  expect_length(series, 1L)
  result <- nbs_fetch_dataset(
    dataset_id, list(series = series),
    client = nbs_client(transport = function(request) {
      stop("longitudinal migrant-worker history must not use the network")
    })
  )
  expect_equal(nrow(result), 18L)
  expect_identical(result$date, as.Date(sprintf("%d-01-01", 2008:2025)))
  expect_equal(result$value[c(3L, 13L, 18L)], c(24223, 28560, 30115))
  expect_true(all(result$unit == "10,000 persons"))
  expect_true(all(result$route == "retained_longitudinal_series"))
  expect_true(all(result$data_status == "latest_retained_official_observation"))
  expect_true(all(grepl("^[0-9a-f]{64}$", result$raw_sha256)))
})

test_that("longitudinal generator is deterministic and source bounded", {
  archive_path <- readNBS:::.nbs_extdata_path(
    "nbs-longitudinal-observations.csv.gz"
  )
  index <- readNBS:::.nbs_load_release_index()
  index <- index[index$dataset_id %in% c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  ), , drop = FALSE]
  archive_hash <- digest::digest(
    file = archive_path, algo = "sha256", serialize = FALSE
  )
  expect_true(nrow(index) > 0L)
  expect_setequal(index$dataset_id, c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  ))
  expect_true(all(index$storage == "snapshot"))
  expect_true(all(index$snapshot_sha256 == archive_hash))
  expect_true(all(index$expected_press_cells > 0L))
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
})
