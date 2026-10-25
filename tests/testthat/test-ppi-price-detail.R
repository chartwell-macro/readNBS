test_that("PPI industry price detail is an exact, classification-aware panel", {
  dataset_id <- "NBS/CN/M/PPI/PRICE_DETAIL"
  catalogue <- nbs_dataset_catalogue("en")
  row <- catalogue[catalogue$dataset_id == dataset_id, , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$route, "portal_uuid_panel")
  expect_equal(row$available_from, as.Date("2007-01-01"))
  expect_equal(row$required_dimensions, "industry|comparison")
  expect_match(row$series_note_en, "Mining of Other Ores", fixed = TRUE)

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 122L)
  expect_true(all(c("industry", "comparison") %in% names(tuples)))
  expect_false(any(
    tuples$industry == "mining_of_other_ores" &
      tuples$comparison == "mom_index"
  ))

  coal_yoy <- nbs_resolve_dataset_series(dataset_id, list(
    industry = "mining_and_washing_of_coal", comparison = "yoy_index"
  ))
  expect_equal(
    coal_yoy$dimension_key,
    "comparison=yoy_index|industry=mining_and_washing_of_coal"
  )
  expect_equal(coal_yoy$available_from, as.Date("2007-01-01"))
  expect_equal(coal_yoy$unit_en, "Index (same month previous year=100)")
  expect_true(grepl(
    "NBS/CN/M/PPI/INDUSTRY/MINING_AND_WASHING_OF_COAL/YOY_INDEX",
    readNBS:::.nbs_dataset_portal_series_id(dataset_id, list(
      industry = "mining_and_washing_of_coal", comparison = "yoy_index"
    )),
    fixed = TRUE
  ))

  expect_error(
    nbs_resolve_dataset_series(dataset_id, list(
      industry = "mining_of_other_ores", comparison = "mom_index"
    )),
    class = "nbs_dataset_filter_error"
  )
})

test_that("PPI coal year-on-year retrieves the verified 2026 portal binding", {
  client <- nbs_client(
    transport = make_observation_transport(list(
      "8bc27b5fd28e46df9b8fda8a5d336306::202601MM-202601MM" =
        "ppi-coal-yoy-2026.json"
    )),
    retries = 0L,
    now = fixed_now
  )
  result <- nbs_fetch_dataset(
    "NBS/CN/M/PPI/PRICE_DETAIL",
    filters = list(
      industry = "mining_and_washing_of_coal", comparison = "yoy_index"
    ),
    start = "2026-01", end = "2026-01", client = client
  )

  expect_equal(result$value, 90.2)
  expect_equal(result$classification_id, "ppi-yoy-2026-")
  expect_equal(result$catalog_uuid, "8bc27b5fd28e46df9b8fda8a5d336306")
  expect_equal(result$indicator_uuid, "ff9c957bd7374bb1ade1ad90b996d71f")
  expect_equal(result$geography, "National")
})

test_that("PPI coal preserves exact historic UUID slices", {
  coal <- nbs_manifest()
  coal <- coal[
    coal$series_id == "NBS/CN/M/PPI/INDUSTRY/MINING_AND_WASHING_OF_COAL/YOY_INDEX",
    , drop = FALSE
  ]
  expect_equal(nrow(coal), 4L)
  expect_equal(coal$slice_start, c(
    "2007-01-01", "2014-01-01", "2018-01-01", "2026-01-01"
  ))
  expect_equal(coal$catalog_uuid, c(
    "8c707b6da5df4703abd7278835e8e668",
    "8c1e3ad3a3a449559f9447918450e193",
    "a9a6bab463334e6c8dc2c66cf114a25e",
    "8bc27b5fd28e46df9b8fda8a5d336306"
  ))
  expect_equal(coal$classification_id, c(
    "ppi-yoy-2007-2013", "ppi-yoy-2014-2017",
    "ppi-yoy-2018-2025", "ppi-yoy-2026-"
  ))
})
