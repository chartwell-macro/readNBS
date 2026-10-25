test_that("migrant-worker history retains every bounded native release", {
  index <- readNBS:::.nbs_load_migrant_history_release_index()
  archive <- readNBS:::.nbs_load_migrant_history_observations()

  expect_identical(index$period, as.character(2012:2024))
  expect_equal(nrow(index), 13L)
  expect_equal(sum(index$expected_press_cells), 2787L)
  expect_equal(nrow(archive), 2787L)
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
  expect_equal(length(unique(index$snapshot_sha256)), 1L)
  expect_true(all(is.finite(archive$value)))
  expect_equal(anyDuplicated(archive$measure), 0L)
  expect_setequal(unique(archive$schema_version), index$schema_version)
})

test_that("migrant-worker history raw captures match release provenance", {
  index <- readNBS:::.nbs_load_migrant_history_release_index()
  capture_dir <- file.path("inst", "extdata", "nbs-migrant-history")
  if (!dir.exists(capture_dir)) {
    capture_dir <- system.file(
      "extdata", "nbs-migrant-history", package = "readNBS", mustWork = FALSE
    )
  }
  paths <- file.path(capture_dir, paste0(index$period, ".html"))
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

test_that("migrant-worker history is an archive, not a 2025 selector backcast", {
  archive <- readNBS:::.nbs_load_migrant_history_observations()
  expect_false(any(archive$schema_version == "migrant-monitoring-2025"))
  expect_true(all(grepl("^release_[0-9]{4}_table_", archive$measure)))
  expect_true(all(archive$release_period %in% as.character(2012:2024)))
  expect_true(any(format(archive$observation_date, "%Y") == "2010"))
})

test_that("mixed native headings resolve every row and column to its actual unit", {
  archive <- readNBS:::.nbs_load_migrant_history_observations()
  archive$key <- paste(archive$release_period, archive$table_index, sep = ":")
  expect_counts <- function(expected, units) {
    for (key in names(expected)) {
      rows <- archive[archive$key == key, , drop = FALSE]
      actual <- table(factor(
        rows$unit_en, levels = units
      ))
      expect_equal(nrow(rows), sum(expected[[key]]), info = key)
      expect_true(all(rows$unit_en %in% units), info = key)
      expect_identical(
        unname(as.integer(actual)), unname(as.integer(expected[[key]])),
        info = key
      )
    }
  }

  headcount_rate <- list(
    "2013:6" = c(12L, 12L), "2013:7" = c(18L, 18L),
    "2015:1" = c(9L, 3L), "2016:1" = c(27L, 9L),
    "2016:2" = c(15L, 15L)
  )
  for (year in 2017:2024) {
    headcount_rate[[paste0(year, ":1")]] <- c(15L, 15L)
    headcount_rate[[paste0(year, ":2")]] <- c(27L, 9L)
  }
  expect_counts(headcount_rate, c("10,000 persons", "Percent"))
  implicit_headcount_rate <- list(
    "2014:5" = c(12L, 12L), "2014:6" = c(18L, 18L),
    "2015:4" = c(12L, 12L), "2015:5" = c(18L, 18L)
  )
  expect_counts(implicit_headcount_rate, c("10,000 persons", "Percent"))

  rate_change <- list(
    "2014:7" = c(18L, 9L), "2014:13" = c(14L, 7L),
    "2014:14" = c(18L, 18L), "2014:15" = c(18L, 18L),
    "2014:16" = c(36L, 36L), "2015:6" = c(18L, 9L),
    "2015:13" = c(14L, 7L), "2016:6" = c(18L, 9L),
    "2016:10" = c(14L, 7L), "2017:6" = c(20L, 10L),
    "2018:4" = c(22L, 11L), "2019:4" = c(20L, 10L),
    "2020:4" = c(20L, 10L), "2021:4" = c(20L, 10L),
    "2022:4" = c(18L, 9L), "2023:4" = c(18L, 9L),
    "2024:4" = c(18L, 9L)
  )
  expect_counts(rate_change, c("Percent", "Percentage points"))

  yuan_rate <- list()
  for (key in c(
    "2014:9", "2015:8", "2016:7", "2017:7", "2018:5",
    paste0(2019:2024, ":5")
  )) yuan_rate[[key]] <- c(14L, 7L)
  expect_counts(
    yuan_rate,
    c("Yuan per worker per month", "Percent")
  )
  # The 2013 consumption table is person-based rather than worker-income based.
  consumption <- archive[archive$key == "2013:8", , drop = FALSE]
  expect_equal(sum(consumption$unit_en == "Yuan per person per month"), 8L)
  expect_equal(sum(consumption$unit_en == "Percent"), 8L)
  expense_rate <- list(
    "2014:10" = c(16L, 8L), "2015:9" = c(16L, 8L),
    "2015:10" = c(16L, 8L)
  )
  expect_counts(expense_rate, c("Yuan per person per month", "Percent"))

  duration_rate <- setNames(
    rep(list(c(2L, 2L, 2L, 4L)), 4L),
    c("2013:10", "2014:11", "2015:11", "2016:8")
  )
  expect_counts(
    duration_rate,
    c("Months per year", "Days per month", "Hours per day", "Percent")
  )

  all_keys <- unique(archive$key)
  mixed_keys <- all_keys[vapply(all_keys, function(key) {
    length(unique(archive$unit_en[archive$key == key])) > 1L
  }, logical(1L))]
  reviewed_keys <- c(
    names(headcount_rate), names(implicit_headcount_rate), names(rate_change),
    names(yuan_rate), "2013:8", names(expense_rate), names(duration_rate)
  )
  expect_setequal(mixed_keys, reviewed_keys)

  table_14_change <- sprintf(
    "release_2014_table_14_row_007_column_%03d", 2:7
  )
  regression <- archive[match(table_14_change, archive$measure), , drop = FALSE]
  expect_identical(regression$measure, table_14_change)
  expect_true(all(regression$row_label == "比上年增加"))
  expect_true(all(regression$unit_en == "Percentage points"))
})

test_that("every migrant-history selector has complete English metadata", {
  dataset_id <- "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"
  archive <- readNBS:::.nbs_load_migrant_history_observations()
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  measures <- dimensions[dimensions$dimension == "measure", , drop = FALSE]
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  cjk <- "[\u3400-\u4DBF\u4E00-\u9FFF]"
  units <- c(
    "10,000 persons", "Percent", "Percentage points",
    "Yuan per worker per month", "Yuan per person per month",
    "Months per year", "Days per month", "Hours per day"
  )

  expect_equal(nrow(measures), 2787L)
  expect_equal(nrow(tuples), 2787L)
  expect_identical(measures$value, archive$measure)
  expect_identical(measures$label_zh, archive$dimension_label_zh)
  expect_identical(measures$label_en, archive$dimension_label_en)
  expect_identical(measures$unit_en, archive$unit_en)
  expect_setequal(unique(measures$unit_en), units)
  expect_equal(anyDuplicated(measures$label_en), 0L)
  expect_false(any(grepl(cjk, measures$label_en, perl = TRUE)))
  expect_false(any(grepl(cjk, measures$unit_en, perl = TRUE)))
  expect_false(any(grepl(
    "^Unit$|Published unit|official published unit|varies|various",
    measures$unit_en, ignore.case = TRUE
  )))
  expect_false(any(grepl("outside HTML grid", archive$source_table, fixed = TRUE)))

  for (i in seq_len(nrow(tuples))) {
    filters <- as.list(tuples[i, c("schema_version", "measure"), drop = FALSE])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
    info <- tuples$dimension_key[[i]]
    expect_equal(nrow(descriptor), 1L, info = info)
    expect_identical(descriptor$dimension_key[[1L]], info, info = info)
    expect_false(grepl(cjk, descriptor$title_en[[1L]], perl = TRUE), info = info)
    expect_identical(descriptor$unit_en[[1L]], measures$unit_en[[i]], info = info)
  }
})
