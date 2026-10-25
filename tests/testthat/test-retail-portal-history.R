test_that("retail underlying portal panel exposes exact reviewed leaves", {
  dataset_id <- "NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING"
  catalogue <- nbs_dataset_catalogue()
  row <- catalogue[catalogue$dataset_id == dataset_id, , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$route, "portal_uuid_panel")
  expect_equal(row$expected_rows, 131L)
  expect_match(row$period_policy, "jan_feb")

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 131L)
  expect_true(all(grepl("^NBS/CN/M/RETAIL/", tuples$series_id)))
  expect_equal(anyDuplicated(tuples$series_id), 0L)
})

test_that("retail underlying panel resolves only to the exact scalar leaf", {
  dataset_id <- "NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING"
  series_id <- "NBS/CN/M/RETAIL/TOTAL/CURRENT_LEVEL"
  resolved <- nbs_resolve_dataset_series(
    dataset_id, list(series_id = series_id)
  )
  expect_equal(resolved$dataset_id, dataset_id)
  expect_equal(resolved$route, "portal_uuid_series")
  expect_equal(resolved$available_from, as.Date("2010-03-01"))
  expect_equal(
    resolved$period_policy,
    "current_january_february_not_separately_reported"
  )

  map <- readNBS:::.nbs_load_dataset_portal_map()
  map <- map[map$dataset_id == dataset_id, , drop = FALSE]
  expect_equal(nrow(map), 131L)
  expect_true(all(map$route_type == "exact"))
  expect_true(all(map$series_id %in% nbs_catalogue()$series_id))
})
