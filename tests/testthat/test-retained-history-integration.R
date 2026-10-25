test_that("capacity history datasets are searchable and fetch offline", {
  annual_id <- "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
  quarterly_id <- paste0(
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/",
    "NATIONAL_HEADLINE_2017_2019"
  )
  catalogue <- nbs_dataset_catalogue()
  expect_true(all(c(annual_id, quarterly_id) %in% catalogue$dataset_id))

  client <- nbs_client(transport = function(request) {
    stop("retained capacity history must not use the network")
  })
  annual_filters <- list(series_id = paste0(
    annual_id, "/NATIONAL_TOTAL/CAPACITY_UTILISATION_PERCENT"
  ))
  annual_descriptor <- nbs_resolve_dataset_series(annual_id, annual_filters)
  annual <- nbs_fetch_dataset(annual_id, annual_filters, client = client)
  expect_equal(nrow(annual), 17L)
  expect_equal(
    annual$value[annual$period %in% as.character(2010:2012)],
    c(79.2, 80.4, 77.5)
  )
  expect_identical(annual_descriptor$unit[[1L]], "Percent")
  expect_identical(
    annual_descriptor$source_vintage[[1L]],
    "retained_official_chart_history"
  )
  expect_identical(annual_descriptor$route[[1L]], "press_release_table")
  expect_true(all(annual$unit == annual_descriptor$unit[[1L]]))
  expect_true(all(
    annual$source_vintage == annual_descriptor$source_vintage[[1L]]
  ))
  expect_true(all(
    readNBS:::.nbs_dataset_observation_provenance_fields %in% names(annual)
  ))

  quarterly_filters <- list(series_id = paste0(
    quarterly_id, "/NATIONAL_TOTAL/CAPACITY_UTILISATION_PERCENT"
  ))
  quarterly_descriptor <- nbs_resolve_dataset_series(
    quarterly_id, quarterly_filters
  )
  quarterly <- nbs_fetch_dataset(
    quarterly_id, quarterly_filters, client = client
  )
  expect_equal(nrow(quarterly), 3L)
  expect_setequal(quarterly$period, c("2017-Q4", "2018-Q1", "2019-Q1"))
  expect_identical(quarterly_descriptor$unit[[1L]], "Percent")
  expect_identical(
    quarterly_descriptor$source_vintage[[1L]],
    "retained_first_release_headlines"
  )
  expect_identical(quarterly_descriptor$route[[1L]], "press_release_table")
  expect_true(all(quarterly$unit == quarterly_descriptor$unit[[1L]]))
  expect_true(all(
    quarterly$source_vintage == quarterly_descriptor$source_vintage[[1L]]
  ))
  expect_true(all(
    readNBS:::.nbs_dataset_observation_provenance_fields %in%
      names(quarterly)
  ))
})

test_that("native migrant-worker history is a searchable offline archive", {
  dataset_id <- "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"
  catalogue <- nbs_dataset_catalogue()
  expect_true(dataset_id %in% catalogue$dataset_id)
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 2787L)

  archive <- readNBS:::.nbs_load_migrant_history_observations()
  expect_identical(min(archive$observation_date), as.Date("2008-01-01"))
  expect_identical(
    catalogue$available_from[catalogue$dataset_id == dataset_id],
    as.Date("2008-01-01")
  )
  selected <- archive[archive$observation_date == as.Date("2008-01-01"), , drop = FALSE]
  if (!nrow(selected)) selected <- archive[1L, , drop = FALSE]
  filters <- as.list(selected[1L, c("schema_version", "measure"), drop = FALSE])
  result <- nbs_fetch_dataset(
    dataset_id, filters, start = "2008", end = "2024",
    client = nbs_client(transport = function(request) {
      stop("retained migrant history must not use the network")
    })
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$date, as.Date("2008-01-01"))
  expect_true(result$date <= as.Date("2024-12-31"))
  expect_equal(result$data_status, "first_release_snapshot")
})

test_that("dataset-scoped archives avoid inflating unrelated history", {
  capacity <- readNBS:::.nbs_press_observations_for_dataset(
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY"
  )
  expect_equal(nrow(capacity), 20L)
  expect_false(any(grepl("HOUSING", capacity$dataset_id, fixed = TRUE)))

  wages <- readNBS:::.nbs_press_observations_for_dataset(
    "NBS/CN/A/WAGES/PRIVATE_HISTORY"
  )
  expect_true(nrow(wages) > 0L)
  expect_setequal(
    unique(wages$dataset_id),
    c("NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", "NBS/CN/A/WAGES/PRIVATE_HISTORY")
  )
})
