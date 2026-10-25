test_that("bundled manifest is stable, bilingual and non-overlapping", {
  manifest <- nbs_manifest()

  # Exact supplemental portal panels extend the core catalogue without
  # changing its scalar identities or validation rules.
  expect_gte(nrow(manifest), 876L)
  expect_gte(length(unique(manifest$series_id)), 639L)
  expect_true("2026-08-03.1" %in% unique(manifest$manifest_version))
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$catalog_uuid)))

  cpi <- manifest[
    manifest$series_id == "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(cpi), 4L)
  expect_equal(
    cpi$classification_id,
    c(
      "cpi-through-2015",
      "cpi-2016-2020",
      "cpi-2021-2025",
      "cpi-2026-"
    )
  )
  expect_match(cpi$series_note_en[[1L]], "January 2016.*January 2021.*January 2026")
  expect_equal(unique(cpi$stitch_policy), "explicit_headline_continuity")
  expect_equal(unique(cpi$period_policy), "all_periods")
  expect_equal(unique(cpi$default_start), "2010-01-01")

  gdp <- manifest[
    manifest$series_id == "NBS/CN/Q/GDP/NOMINAL/CURRENT_QUARTER",
    ,
    drop = FALSE
  ]
  expect_match(unique(gdp$series_note_en), "2015 Q3", fixed = TRUE)
  expect_match(unique(gdp$series_note_en), "revised current-quarter history")
  expect_equal(cpi$slice_start[[1L]], "2010-01-01")

  retail <- manifest[
    manifest$series_id == "NBS/CN/M/RETAIL/TOTAL/YOY_PERCENT",
    ,
    drop = FALSE
  ]
  expect_equal(
    unique(retail$period_policy),
    "current_january_february_not_separately_reported"
  )
})

test_that("manifest overlap and incompatible semantics fail explicitly", {
  manifest <- nbs_manifest()
  cpi_rows <- which(
    manifest$series_id == "NBS/CN/M/CPI/HEADLINE/YOY_INDEX"
  )

  overlap <- manifest
  overlap$slice_start[cpi_rows[[2L]]] <- "2015-12-01"
  expect_error(
    readNBS:::.nbs_validate_manifest(overlap),
    class = "nbs_manifest_overlap_error"
  )

  incompatible <- manifest
  incompatible$unit_en[cpi_rows[[2L]]] <- "Percent"
  expect_error(
    readNBS:::.nbs_validate_manifest(incompatible),
    class = "nbs_manifest_error"
  )
})

test_that("catalogue and search are local, bilingual and bounded", {
  calls <- 0L
  client <- nbs_client(
    transport = function(request) {
      calls <<- calls + 1L
      stop("network must not be called")
    },
    now = fixed_now
  )
  expect_s3_class(client, "nbs_client")
  expect_equal(calls, 0L)

  catalogue <- nbs_catalogue("en")
  expect_gte(nrow(catalogue), 642L)
  expect_equal(calls, 0L)
  expect_match(
    catalogue$series_note[
      catalogue$series_id == "NBS/CN/M/CPI/HEADLINE/YOY_INDEX"
    ],
    "January 2026"
  )
  expect_equal(
    catalogue$available_from[
      catalogue$series_id == "NBS/CN/M/CPI/HEADLINE/YOY_INDEX"
    ],
    as.Date("2010-01-01")
  )

  expect_true(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX" %in% nbs_search("CPI")$series_id
  )
  expect_true(
    "NBS/CN/A/POPULATION/TOTAL/YEAR_END" %in%
      nbs_search("人口", language = "zh")$series_id
  )
  expect_lte(nrow(nbs_search("", limit = 2L)), 2L)
  expect_error(nbs_search("", limit = 101L), class = "nbs_argument_error")
  expect_setequal(
    unique(catalogue$release_id[catalogue$family == "households"]),
    "cn_nbs_households"
  )
  expect_setequal(
    unique(catalogue$release_id[catalogue$family == "population"]),
    "cn_nbs_demographics"
  )
  expect_equal(calls, 0L)
})

test_that("catalogue bounds agree with every manifest slice", {
  manifest <- nbs_manifest()
  catalogue <- nbs_catalogue("en")
  direct <- catalogue[catalogue$route == "portal_uuid_series", , drop = FALSE]
  ids <- unique(manifest$series_id)
  expect_identical(direct$series_id, ids)
  expected_from <- vapply(ids, function(series_id) {
    min(as.Date(manifest$slice_start[manifest$series_id == series_id]))
  }, as.Date(NA))
  expected_to <- vapply(ids, function(series_id) {
    ends <- manifest$slice_end[manifest$series_id == series_id]
    if (any(!nzchar(ends))) return(as.Date(NA))
    max(as.Date(ends))
  }, as.Date(NA))
  expect_identical(direct$available_from, as.Date(unname(expected_from)))
  expect_identical(direct$available_to, as.Date(unname(expected_to)))
})

test_that("release-calendar policies preserve official January omissions", {
  expected <- readNBS:::.nbs_expected_period_sequence(
    as.Date("2025-12-01"),
    as.Date("2026-03-01"),
    "M",
    "omit_january"
  )
  expect_equal(
    expected,
    as.Date(c("2025-12-01", "2026-02-01", "2026-03-01"))
  )
  expect_equal(
    readNBS:::.nbs_expected_period_sequence(
      as.Date("2025-12-01"),
      as.Date("2026-03-01"),
      "M",
      "omit_january_february"
    ),
    as.Date(c("2025-12-01", "2026-03-01"))
  )
})
