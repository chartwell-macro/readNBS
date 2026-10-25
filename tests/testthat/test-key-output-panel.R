test_that("key industrial and energy outputs expose long official histories", {
  dataset_id <- "NBS/CN/M/INDUSTRIAL_OUTPUT/KEY_PRODUCTS"
  catalogue <- nbs_dataset_catalogue("en")
  row <- catalogue[catalogue$dataset_id == dataset_id, , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_identical(row$expected_rows, 20L)
  expect_identical(row$available_from, as.Date("2010-02-01"))
  expect_identical(row$available_to, as.Date("2026-06-01"))

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 20L)
  expected_products <- c(
    "ELECTRICITY", "CEMENT", "PLAIN_GLASS", "CRUDE_STEEL", "ROLLED_STEEL"
  )
  for (product in expected_products) {
    expect_equal(sum(grepl(paste0("/", product, "/"), tuples$series_id)), 4L)
  }
  manifest <- nbs_manifest()
  members <- manifest[manifest$series_id %in% tuples$series_id, ]
  expected_observations <- vapply(seq_len(nrow(members)), function(index) {
    length(readNBS:::.nbs_expected_period_sequence(
      as.Date(members$slice_start[[index]]),
      as.Date(members$slice_end[[index]]),
      members$frequency[[index]],
      members$period_policy[[index]]
    ))
  }, integer(1L))
  expect_gte(min(expected_observations), 164L)

  ytd <- nbs_resolve_dataset_series(
    dataset_id,
    list(series_id = "NBS/CN/M/OUTPUT/CEMENT/YTD_LEVEL"),
    "en"
  )
  expect_identical(ytd$available_from, as.Date("2010-02-01"))
  expect_identical(ytd$unit, "10,000 tonnes")
  expect_identical(ytd$aggregation_basis, "year_to_date")
  expect_identical(
    ytd$period_policy,
    "ytd_january_not_reported_february_combined"
  )

  current <- nbs_resolve_dataset_series(
    dataset_id,
    list(series_id = "NBS/CN/M/OUTPUT/CEMENT/CURRENT_LEVEL"),
    "en"
  )
  expect_identical(current$available_from, as.Date("2010-03-01"))
  expect_identical(current$aggregation_basis, "single_period")
  expect_identical(
    current$period_policy,
    "current_january_february_not_separately_reported"
  )
})
