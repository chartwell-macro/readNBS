test_that("industrial longitudinal panels are conservative contiguous mappings", {
  catalogue <- nbs_dataset_catalogue("en")
  expected <- c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL" = 2150L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL" = 208L
  )
  rows <- catalogue[match(names(expected), catalogue$dataset_id), ]

  expect_identical(rows$expected_rows, unname(expected))
  expect_identical(rows$route, rep("portal_uuid_panel", 2L))
  expect_identical(rows$available_from, rep(as.Date("2010-02-01"), 2L))
  for (dataset_id in names(expected)) {
    expect_equal(nrow(nbs_dataset_valid_tuples(dataset_id)), expected[[dataset_id]])
  }

  manifest <- utils::read.csv(
    gzfile(readNBS:::.nbs_extdata_path(
      "nbs-industrial-longitudinal-series-manifest.csv.gz"
    )),
    stringsAsFactors = FALSE
  )
  groups <- split(manifest, manifest$series_id)
  expect_equal(length(groups), 2358L)
  expect_equal(nrow(manifest), 5578L)
  expect_true(all(lengths(groups) >= 2L))
  expect_true(all(vapply(groups, function(rows) {
    length(unique(rows$title_en)) == 1L &&
      length(unique(rows$unit_en)) == 1L &&
      length(unique(rows$period_policy)) == 1L &&
      length(unique(rows$aggregation_basis)) == 1L
  }, logical(1L))))
  expected_observations <- vapply(groups, function(rows) {
    length(readNBS:::.nbs_expected_period_sequence(
      min(as.Date(rows$slice_start)),
      max(as.Date(rows$slice_end)),
      rows$frequency[[1L]],
      rows$period_policy[[1L]]
    ))
  }, integer(1L))
  expect_gte(min(expected_observations), 66L)
  expect_equal(sum(expected_observations < 24L), 0L)
})

test_that("headline industrial profits span all adjacent official eras", {
  dataset_id <-
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL"
  series_id <- paste0(
    "NBS/CN/M/INDUSTRIAL_ENTERPRISES/LONGITUDINAL/INDUSTRY/",
    "9FDB99AA749C264FFED3/YTD_LEVEL"
  )
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_true(series_id %in% tuples$series_id)

  descriptor <- nbs_resolve_dataset_series(
    dataset_id, list(series_id = series_id), "en"
  )
  expect_identical(descriptor$available_from, as.Date("2010-02-01"))
  expect_identical(descriptor$available_to, as.Date("2026-06-01"))
  expect_identical(descriptor$aggregation_basis, "year_to_date")
  expect_identical(
    descriptor$period_policy,
    "ytd_january_not_reported_february_combined"
  )

  slices <- nbs_manifest()
  slices <- slices[slices$series_id == series_id, ]
  expect_identical(
    slices$classification_id,
    c(
      "gb-t-4754-2002-2003-2011",
      "gb-t-4754-2011-2012-2017",
      "gb-t-4754-2017-2018-present"
    )
  )
  expect_silent(readNBS:::.nbs_slice_intersections(
    slices,
    as.Date("2011-12-01"),
    as.Date("2012-02-01")
  ))
})
