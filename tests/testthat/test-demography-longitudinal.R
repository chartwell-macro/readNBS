test_that("longitudinal demography exposes the reviewed annual portal panel", {
  dataset_id <- "NBS/CN/A/DEMOGRAPHY/LONGITUDINAL"
  catalogue <- nbs_dataset_catalogue("en")
  row <- catalogue[catalogue$dataset_id == dataset_id, , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_identical(row$route, "portal_uuid_panel")
  expect_identical(row$available_from, as.Date("1949-01-01"))
  expect_identical(row$expected_rows, 14L)

  tuples <- nbs_dataset_valid_tuples(dataset_id)
  expect_equal(nrow(tuples), 14L)
  expect_true(
    "NBS/CN/A/POPULATION/TOTAL/YEAR_END" %in% tuples$series_id
  )
  manifest <- nbs_manifest()
  members <- manifest[manifest$series_id %in% tuples$series_id, ]
  expected_observations <- vapply(seq_len(nrow(members)), function(index) {
    end <- if (nzchar(members$slice_end[[index]])) {
      as.Date(members$slice_end[[index]])
    } else {
      as.Date("2025-01-01")
    }
    length(readNBS:::.nbs_expected_period_sequence(
      as.Date(members$slice_start[[index]]),
      end,
      members$frequency[[index]],
      members$period_policy[[index]]
    ))
  }, integer(1L))
  expect_gte(min(expected_observations), 26L)

  filters <- list(series_id = "NBS/CN/A/POPULATION/TOTAL/YEAR_END")
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
  expect_identical(descriptor$available_from, as.Date("1949-01-01"))
  expect_identical(descriptor$unit, "10,000 persons")

  result <- nbs_fetch_dataset(
    dataset_id,
    filters,
    start = "2025",
    end = "2025",
    client = nbs_client(
      transport = make_observation_transport(list(
        "6331ad868e8b4f55b8e9b6e765609ce1::2025YY-2025YY" =
          "population-2025.json"
      )),
      retries = 0L,
      now = fixed_now
    )
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$dataset_id, dataset_id)
  expect_identical(result$dimension_key, nbs_dimension_key(filters))
  expect_equal(result$value, 140489)
})
