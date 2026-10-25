test_that("snapshot coverage is cached for the exact dimensional tuple", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  beijing_new <- list(
    city_code = "110000",
    market = "new",
    size_band = "all",
    comparison = "mom",
    index_base = "2025"
  )
  dali_second_hand <- list(
    city_code = "532900",
    market = "second_hand",
    size_band = "gt144",
    comparison = "fixed_base",
    index_base = "2010"
  )

  first <- readNBS:::.nbs_dataset_snapshot_coverage(dataset_id, beijing_new)
  second <- readNBS:::.nbs_dataset_snapshot_coverage(
    dataset_id,
    dali_second_hand
  )

  expect_identical(first, list(
    available_from = as.Date("2026-01-01"),
    available_to = as.Date("2026-06-30")
  ))
  expect_identical(second, list(
    available_from = as.Date("2011-02-01"),
    available_to = as.Date("2015-12-31")
  ))
  continuous <- readNBS:::.nbs_dataset_snapshot_coverage(
    dataset_id,
    within(beijing_new, index_base <- "all_official_bases")
  )
  expect_identical(continuous, list(
    available_from = as.Date("2011-01-01"),
    available_to = as.Date("2026-06-30")
  ))
  expect_identical(
    readNBS:::.nbs_dataset_snapshot_coverage(dataset_id, beijing_new),
    first
  )
})

test_that("vectorized tuple keys retain the public canonical identity", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  required <- setdiff(names(tuples), "dimension_key")
  sample_rows <- unique(c(1L, nrow(tuples) %/% 2L, nrow(tuples)))

  expected <- vapply(sample_rows, function(index) {
    nbs_dimension_key(as.list(tuples[index, required, drop = FALSE]))
  }, character(1L))

  expect_identical(tuples$dimension_key[sample_rows], expected)
  expect_identical(anyDuplicated(tuples$dimension_key), 0L)
})

test_that("vectorized coverage recognises continuous housing change tuples", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  continuous <- tuples[
    tuples$index_base == "all_official_bases", , drop = FALSE
  ]
  coverage <- readNBS:::.nbs_dataset_snapshot_coverages(
    dataset_id, continuous
  )

  expect_equal(nrow(continuous), 1680L)
  expect_true(all(
    coverage$available_from[
      continuous$comparison != "ytd_avg_yoy" & continuous$size_band == "all"
    ] ==
      as.Date("2011-01-01")
  ))
  expect_true(all(
    coverage$available_from[
      continuous$comparison != "ytd_avg_yoy" & continuous$size_band != "all"
    ] ==
      as.Date("2011-02-01")
  ))
  expect_true(all(
    coverage$available_from[continuous$comparison == "ytd_avg_yoy"] ==
      as.Date("2023-01-01")
  ))
  expect_true(all(coverage$available_to == as.Date("2026-06-30")))
  expect_false(any(coverage$schema_era == "manifest_default"))
})
