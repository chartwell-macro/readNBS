test_that("industrial portal panels retain official registration and industry eras", {
  expected <- c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023" = 520L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024" = 220L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002" = 1960L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011" = 2418L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017" = 3002L
  )
  catalogue <- nbs_dataset_catalogue()
  rows <- catalogue[match(names(expected), catalogue$dataset_id), , drop = FALSE]
  expect_equal(rows$expected_rows, unname(expected))
  expect_equal(rows$route, rep("portal_uuid_panel", length(expected)))

  legacy <- nbs_dataset_valid_tuples(names(expected)[[1L]])
  current <- nbs_dataset_valid_tuples(names(expected)[[2L]])
  expect_equal(nrow(legacy), expected[[1L]])
  expect_equal(nrow(current), expected[[2L]])
  expect_false(any(legacy$series_id %in% current$series_id))

  industry <- nbs_dataset_valid_tuples(names(expected)[[3L]])
  expect_equal(nrow(industry), expected[[3L]])
  expect_false(any(industry$series_id %in% legacy$series_id))
  industry_2011 <- nbs_dataset_valid_tuples(names(expected)[[4L]])
  industry_2017 <- nbs_dataset_valid_tuples(names(expected)[[5L]])
  expect_equal(nrow(industry_2011), expected[[4L]])
  expect_equal(nrow(industry_2017), expected[[5L]])
  expect_false(any(industry$series_id %in% industry_2011$series_id))
  expect_false(any(industry_2011$series_id %in% industry_2017$series_id))

  expect_equal(rows$available_from[3:5], as.Date(c(
    "2010-02-01", "2012-02-01", "2018-02-01"
  )))
  expect_equal(rows$available_to[3:5], as.Date(c(
    "2011-12-01", "2017-12-01", "2026-06-01"
  )))
})

test_that("industrial portal history resolves only exact UUID bindings", {
  dataset_id <- "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002"
  tuple <- nbs_dataset_valid_tuples(dataset_id)[1L, , drop = FALSE]
  resolved <- nbs_resolve_dataset_series(
    dataset_id, list(series_id = tuple$series_id[[1L]])
  )
  expect_equal(resolved$dataset_id, dataset_id)
  scalar <- nbs_manifest()
  scalar <- scalar[scalar$series_id == tuple$series_id[[1L]], , drop = FALSE]
  expect_equal(scalar$classification_id, "gb-t-4754-2002-2003-2011")
  expect_equal(resolved$available_from, as.Date("2010-02-01"))
  expect_equal(resolved$available_to, as.Date("2011-12-01"))
  expect_match(scalar$stitch_policy, "no_cross_era")
})

test_that("legacy registration UUID labels retain their distinct meanings", {
  dataset_id <-
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023"
  prefix <- paste0(
    "NBS/CN/M/INDUSTRIAL_PORTAL/REGISTRATION_TYPE_THROUGH_2023/",
    "MAIN_ECONOMIC_INDICATORS_OF_COLLECTIVE_OWNED_INDUSTRIAL_ENTERPRISES_2018/"
  )
  suffixes <- c(
    "C3F3B6AB00474CAA8D972C98E86C3B7C",
    "363F3D8F6D5D48AD9F495B99BEA879B8",
    "BDAE32CF49F54822BA09942AF4633CB5",
    "8E5C303DDD2C45B99FDF152344CBC02D",
    "690AEEC30DBD4436A5C7FCE49AE48324"
  )
  series_ids <- paste0(prefix, suffixes)
  expected <- c(
    "Total Current Assets of Collective-owned Industrial Enterprises, month-end (100 million yuan)",
    "Financial Costs of Collective-owned Industrial Enterprises, Accumulated (100 million yuan)",
    "Financial Costs of Collective-owned Industrial Enterprises, Accumulated, The Same Period of Previous Year (100 million yuan)",
    "Interest Expenses of Collective-owned Industrial Enterprises, Accumulated (100 million yuan)",
    "Interest Expenses of Collective-owned Industrial Enterprises, Accumulated, The Same Period of Previous Year (100 million yuan)"
  )
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  rows <- dimensions[match(series_ids, dimensions$value), , drop = FALSE]

  expect_identical(rows$value, series_ids)
  expect_identical(rows$route_value, series_ids)
  expect_identical(trimws(rows$label_en), expected)
  expect_identical(rows$semantics, c("end_period", rep("year_to_date", 4L)))
  expect_identical(anyDuplicated(rows$label_en), 0L)

  resolved <- lapply(series_ids, function(series_id) {
    nbs_resolve_dataset_series(dataset_id, list(series_id = series_id))
  })
  titles <- vapply(resolved, function(result) result$title_en[[1L]], "")
  expect_identical(anyDuplicated(titles), 0L)
  expect_true(all(endsWith(titles, expected)))

  manifest <- nbs_manifest()
  leaves <- manifest[match(series_ids, manifest$series_id), , drop = FALSE]
  expect_identical(leaves$indicator_uuid, tolower(suffixes))
  expect_identical(trimws(leaves$title_en), expected)
  expect_identical(
    leaves$aggregation_basis,
    c("single_period", rep("year_to_date", 4L))
  )
  expect_identical(
    leaves$period_policy,
    c(
      "january_not_reported_february_first_period",
      rep("ytd_january_not_reported_february_combined", 4L)
    )
  )
})

test_that("industrial portal UUIDs retain explicit official scalar units", {
  manifest <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-industrial-series-manifest.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  dimensions <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-industrial-dataset-dimensions.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  affected <- dimensions$unit_en == "10,000 persons"
  counts <- table(dimensions$dataset_id[affected])
  expect_equal(as.integer(counts[c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024"
  )]), c(90L, 12L, 8L))
  expect_equal(dimensions$unit_zh[affected], rep("万人", sum(affected)))

  enterprise_counts <- dimensions$unit_en == "Enterprises"
  counts <- table(dimensions$dataset_id[enterprise_counts])
  expect_equal(as.integer(counts[c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024"
  )]), c(120L, 126L, 135L, 21L, 12L))
  expect_equal(dimensions$unit_zh[enterprise_counts],
    rep("家", sum(enterprise_counts)))
  expect_equal(sum(manifest$unit_en == "Enterprises"), 414L)
  expect_equal(manifest$unit_zh[manifest$unit_en == "Enterprises"],
    rep("家", 414L))

  generic_units <- c("Official published unit", "Unit")
  generic_units_zh <- c("官方发布单位", "Unit")
  expect_false(any(manifest$unit_en %in% generic_units))
  expect_false(any(dimensions$unit_en %in% generic_units))
  expect_false(any(manifest$unit_zh %in% generic_units_zh))
  expect_false(any(dimensions$unit_zh %in% generic_units_zh))
})
