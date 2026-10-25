test_that("CPI price detail is an exact, bounded portal panel", {
  dataset_id <- "NBS/CN/M/CPI/PRICE_DETAIL"
  catalogue <- nbs_dataset_catalogue("en")
  row <- catalogue[catalogue$dataset_id == dataset_id, , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$route, "portal_uuid_panel")
  expect_equal(row$available_from, as.Date("2010-01-01"))
  expect_match(row$series_note_en, "sparse food leaves", fixed = TRUE)

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 14L)
  expect_true(all(tuples$residence[tuples$category != "headline"] == "national"))
  expect_true(all(tuples$comparison[tuples$category != "headline"] == "yoy_index"))
  expect_false("pork" %in% nbs_dataset_dimensions(dataset_id)$value)

  urban_mom <- nbs_resolve_dataset_series(dataset_id, list(
    residence = "urban", comparison = "mom_index", category = "headline"
  ))
  expect_equal(
    urban_mom$dimension_key,
    "category=headline|comparison=mom_index|residence=urban"
  )
  expect_equal(urban_mom$available_from, as.Date("2010-01-01"))
  expect_equal(urban_mom$unit_en, "Index (preceding month=100)")
  expect_true(grepl(
    "NBS/CN/M/CPI/URBAN/HEADLINE/MOM_INDEX",
    readNBS:::.nbs_dataset_portal_series_id(
      dataset_id,
      list(residence = "urban", comparison = "mom_index", category = "headline")
    ), fixed = TRUE
  ))

  expect_error(
    nbs_resolve_dataset_series(dataset_id, list(
      residence = "rural", comparison = "yoy_index", category = "grain"
    )),
    class = "nbs_dataset_filter_error"
  )
})

test_that("CPI urban M/M retrieves the verified 2010 portal binding", {
  client <- nbs_client(
    transport = make_observation_transport(list(
      "6d767f323c1b427684637dc1a027cef1::201001MM-201001MM" =
        "cpi-urban-mom-2010.json"
    )),
    retries = 0L,
    now = fixed_now
  )
  result <- nbs_fetch_dataset(
    "NBS/CN/M/CPI/PRICE_DETAIL",
    filters = list(
      residence = "urban", comparison = "mom_index", category = "headline"
    ),
    start = "2010-01", end = "2010-01", client = client
  )

  expect_equal(result$value, 101.5)
  expect_equal(result$classification_id, "cpi-through-2015")
  expect_equal(result$catalog_uuid, "6d767f323c1b427684637dc1a027cef1")
  expect_equal(result$indicator_uuid, "6752d9f16f7c418a9ab55c21e2b1cd8f")
  expect_equal(result$geography, "Urban")
  expect_true(all(
    readNBS:::.nbs_dataset_observation_provenance_fields %in% names(result)
  ))
  expect_identical(result$unit, result$unit_en)
})

test_that("continuous national food components retain every published base era", {
  grain <- nbs_manifest()
  grain <- grain[
    grain$series_id == "NBS/CN/M/CPI/FOOD/GRAIN/YOY_INDEX", , drop = FALSE
  ]
  expect_equal(nrow(grain), 4L)
  expect_equal(grain$slice_start, c(
    "2010-01-01", "2016-01-01", "2021-01-01", "2026-01-01"
  ))
  expect_equal(unique(grain$catalog_uuid), "deccae3d6ecb4967809ab21a3c679c7f")
  expect_equal(unique(grain$indicator_uuid), "622350938e7f457c9c2d2565c727a929")
  expect_equal(grain$classification_id, c(
    "cpi-through-2015", "cpi-2016-2020", "cpi-2021-2025", "cpi-2026-"
  ))
})
