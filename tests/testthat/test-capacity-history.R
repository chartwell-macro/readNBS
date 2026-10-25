test_that("capacity history has a source-faithful annual and early-headline archive", {
  index <- readNBS:::.nbs_load_capacity_history_release_index()
  archive <- readNBS:::.nbs_load_capacity_history_observations()

  expect_identical(index$period, c("2006-2022", "2017-Q4", "2018-Q1", "2019-Q1"))
  expect_identical(index$expected_press_cells, c(17L, 1L, 1L, 1L))
  expect_equal(nrow(archive), 20L)
  expect_true(all(is.finite(archive$value)))
  expect_true(all(archive$seasonal_adjustment == "not_seasonally_adjusted"))
  expect_false(any(archive$release_period %in% c("2018-Q2", "2019-Q2")))
})

test_that("capacity history retains the official chart values and does not invent detailed cells", {
  archive <- readNBS:::.nbs_load_capacity_history_observations()
  annual <- archive[archive$release_period == "2006-2022", ]
  headline <- archive[archive$release_period != "2006-2022", ]

  expect_identical(format(annual$observation_date, "%Y"), as.character(2006:2022))
  expect_equal(annual$value, c(
    79.1, 81.3, 77.2, 73.1, 79.2, 80.4, 77.5, 75.8, 75.6,
    74.3, 73.3, 77.0, 76.5, 76.6, 74.5, 77.5, 75.6
  ))
  expect_equal(headline$value, c(78.0, 76.5, 75.9))
  expect_identical(headline$published_value_type, c(
    "official_subsequent_release_comparison", "official_headline_value",
    "official_subsequent_release_comparison"
  ))
  expect_false(any(grepl("industry|ytd", archive$series_id, ignore.case = TRUE)))
})

test_that("capacity-history raw captures and compressed archive hashes verify offline", {
  index <- readNBS:::.nbs_load_capacity_history_release_index()
  source_index <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-capacity-history-source-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  actual <- vapply(source_index$retained_file, function(file) digest::digest(
    file = readNBS:::.nbs_extdata_path(file.path("nbs-capacity-history", file)),
    algo = "sha256", serialize = FALSE
  ), character(1L))
  expect_identical(unname(actual), source_index$source_capture_sha256)
  expect_equal(length(unique(index$snapshot_sha256)), 1L)
})
