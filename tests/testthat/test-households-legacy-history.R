test_that("pre-unified household survey panels retain every exact annual tuple", {
  datasets <- c(
    "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
    "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012"
  )
  dimensions <- readNBS:::.nbs_load_dataset_dimensions()
  releases <- readNBS:::.nbs_load_release_index()

  for (dataset_id in datasets) {
    expected <- if (grepl("URBAN", dataset_id, fixed = TRUE)) 29L else 33L
    rows <- releases[releases$dataset_id == dataset_id, , drop = FALSE]
    expect_equal(rows$period, c("2011", "2012"))
    expect_true(all(rows$expected_press_cells == expected))
    expect_equal(length(unique(rows$snapshot_sha256)), 1L)

    members <- dimensions[
      dimensions$dataset_id == dataset_id & dimensions$dimension == "series",
      , drop = FALSE
    ]
    expect_equal(nrow(members), expected)
    expect_true(all(members$valid_from == "2011-01-01"))
    expect_true(all(members$valid_to == "2012-12-31"))

    for (series in members$value) {
      result <- nbs_fetch_dataset(dataset_id, list(series = series))
      expect_equal(nrow(result), 2L, info = paste(dataset_id, series))
      expect_equal(format(result$date, "%Y"), c("2011", "2012"))
      expect_true(all(is.finite(result$value)))
      expect_equal(unique(result$classification_id), if (
        grepl("URBAN", dataset_id, fixed = TRUE)
      ) "urban_household_survey_pre2013" else "rural_household_survey_pre2013")
      expect_true(all(grepl("^[0-9a-f]{64}$", result$raw_sha256)))
    }
  }
})

test_that("retained household raw captures match their published SHA-256 provenance", {
  source_index <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-households-legacy-source-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
  )
  paths <- c(
    `2011-urban` = "nbs-households-legacy/2012/J1005C.HTM",
    `2011-rural` = "nbs-households-legacy/2012/J1018C.HTM",
    `2012-urban` = "nbs-households-legacy/2013/Z1104C.xls",
    `2012-rural` = "nbs-households-legacy/2013/Z1117C.xls"
  )
  expected <- c(
    `2011-urban` = source_index$source_capture_sha256[
      source_index$release_period == "2011" & source_index$residence == "urban"
    ],
    `2011-rural` = source_index$source_capture_sha256[
      source_index$release_period == "2011" & source_index$residence == "rural"
    ],
    `2012-urban` = source_index$source_capture_sha256[
      source_index$release_period == "2012" & source_index$residence == "urban"
    ],
    `2012-rural` = source_index$source_capture_sha256[
      source_index$release_period == "2012" & source_index$residence == "rural"
    ]
  )
  source_paths <- file.path("inst", "extdata", paths)
  if (!all(file.exists(source_paths))) {
    installed_dir <- system.file("extdata", package = "readNBS", mustWork = FALSE)
    source_paths <- file.path(installed_dir, paths)
  }
  present <- file.exists(source_paths)
  if (all(present)) {
    actual <- vapply(seq_along(source_paths), function(index) digest::digest(
      file = source_paths[[index]], algo = "sha256", serialize = FALSE
    ), character(1L))
    names(actual) <- names(paths)
    expect_identical(actual[names(expected)], expected)
  } else {
    expect_false(any(present))
  }
})
