test_that("early demographic communiques are frozen with their distinct schemas", {
  index <- readNBS:::.nbs_load_demography_history_release_index()
  observations <- readNBS:::.nbs_load_demography_history_observations()

  expect_equal(index$period, c("2010", "2011"))
  expect_equal(index$expected_press_cells, c(1L, 6L))
  expect_equal(index$schema_version, c(
    "demography-total-population-preliminary-2010-v1",
    "demography-total-population-age-15-59-2011-v1"
  ))
  expect_equal(observations$value, c(
    134100, 134735, 1604, 960, 51.3, 94072, 18499
  ))
  expect_equal(observations$measure, c(
    "total_population_year_end", "total_population_year_end", "births_level",
    "deaths_level", "urbanisation_percent", "age_15_59_year_end",
    "age_60_plus_year_end"
  ))
  expect_equal(observations$raw_value[c(1L, 3L, 4L)], c("134100", "16.04", "9.60"))
})

test_that("early demographic capture hashes and offline exact fetch are reproducible", {
  pages <- c(
    "nbs-demography-history/2010.html",
    "nbs-demography-history/2011.html"
  )
  expected_source_hashes <- c(
    "4e6bc7cbd0d2df2308e5932c113c2d671055b2dacfa675a581c2426e74a21709",
    "46690b985daf1bd9abc528798c3d097aa00d5ab86ad47fb2d63fe64facdd7205"
  )
  source_pages <- file.path("inst", "extdata", pages)
  if (!all(file.exists(source_pages))) {
    installed_dir <- system.file("extdata", package = "readNBS", mustWork = FALSE)
    source_pages <- file.path(installed_dir, pages)
  }
  present <- file.exists(source_pages)
  if (all(present)) {
    actual_source_hashes <- vapply(source_pages, function(page) digest::digest(
      file = page, algo = "sha256", serialize = FALSE
    ), character(1L))
    expect_identical(unname(actual_source_hashes), expected_source_hashes)
  } else {
    expect_false(any(present))
  }

  total <- nbs_fetch_dataset(
    "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
    filters = list(measure = "total_population_year_end"),
    start = "2010-01-01", end = "2011-12-31"
  )
  expect_equal(total$value, c(134100, 134735))
  expect_equal(total$schema_version, c(
    "demography-total-population-preliminary-2010-v1",
    "demography-total-population-age-15-59-2011-v1"
  ))

  births <- nbs_fetch_dataset(
    "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
    filters = list(measure = "births_level"),
    start = "2010-01-01", end = "2011-12-31"
  )
  expect_equal(births$value, 1604)
  expect_equal(births$raw_value, "16.04")
})
