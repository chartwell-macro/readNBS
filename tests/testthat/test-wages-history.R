test_that("retained annual wage-history panels expose source-faithful eras", {
  ids <- c(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY",
    "NBS/CN/A/WAGES/PRIVATE_HISTORY"
  )
  catalogue <- nbs_dataset_catalogue("en")
  rows <- catalogue[catalogue$dataset_id %in% ids, , drop = FALSE]
  expect_equal(nrow(rows), 2L)
  expect_true(all(rows$available_from == as.Date("2010-01-01")))
  expect_true(all(rows$available_to == as.Date("2021-12-31")))

  releases <- readNBS:::.nbs_load_release_index()
  releases <- releases[releases$dataset_id %in% ids, , drop = FALSE]
  expect_equal(nrow(releases), 24L)
  expect_true(all(grepl("^[0-9a-f]{64}$", releases$source_capture_sha256)))
  expect_equal(length(unique(releases$snapshot_sha256)), 1L)
})

test_that("wage history does not stitch industry classifications", {
  base <- list(
    table = "industry", region = "not_applicable", industry = "manufacturing",
    registration = "not_applicable", measure = "average_wage"
  )
  early <- c(base, list(classification = "non_private_employees_gbt4754_2002"))
  gbt2011 <- c(base, list(classification = "gbt4754_2011"))
  gbt2017 <- c(base, list(classification = "gbt4754_2017"))

  a <- nbs_fetch_dataset(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", early,
    start = "2010", end = "2011"
  )
  b <- nbs_fetch_dataset(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", gbt2011,
    start = "2012", end = "2017"
  )
  c <- nbs_fetch_dataset(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", gbt2017,
    start = "2018", end = "2021"
  )
  expect_equal(a$value, c(30700, 36494))
  expect_equal(b$value, c(41650, 46431, 51369, 55324, 59470, 64452))
  expect_equal(c$value, c(72088, 78147, 82783, 92459))
  expect_error(
    nbs_fetch_dataset(
      "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", gbt2017,
      start = "2012", end = "2017"
    ),
    class = "nbs_range_error"
  )
})

test_that("retained wage histories have exact source-table tuple constraints", {
  tuples <- nbs_dataset_valid_tuples("NBS/CN/A/WAGES/PRIVATE_HISTORY")
  expect_true(nrow(tuples) > 40L)
  expect_false(any(tuples$table == "registration"))
  filters <- list(
    classification = "gbt4754_2017", table = "industry", region = "not_applicable",
    industry = "total", registration = "not_applicable",
    measure = "nominal_growth_rate"
  )
  value <- nbs_fetch_dataset(
    "NBS/CN/A/WAGES/PRIVATE_HISTORY", filters,
    start = "2018", end = "2021"
  )
  expect_equal(value$value, c(8.3, 8.1, 7.7, 8.9))
})

test_that("wage-history raw captures are hashed in a source checkout", {
  index <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-wages-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  source_dir <- file.path("inst", "extdata", "nbs-wages-history")
  if (!dir.exists(source_dir)) {
    source_dir <- system.file(
      "extdata", "nbs-wages-history", package = "readNBS", mustWork = FALSE
    )
  }
  paths <- file.path(source_dir, paste0(
    index$period, "-", ifelse(
      index$dataset_id == "NBS/CN/A/WAGES/PRIVATE_HISTORY",
      "private", "non_private"
    ), ".html"
  ))
  present <- file.exists(paths)
  if (all(present)) {
    actual <- vapply(paths, function(path) digest::digest(
      file = path, algo = "sha256", serialize = FALSE
    ), character(1L))
    expect_identical(unname(actual), index$source_capture_sha256)
  } else {
    expect_false(any(present))
  }
})
