test_that("ten-day history retains the official 2014--19 release calendar", {
  index <- readNBS:::.nbs_load_tenday_history_release_index()
  archive <- readNBS:::.nbs_load_tenday_history_observations()

  expect_identical(index$period, readNBS:::.nbs_tenday_history_expected_periods())
  expect_equal(nrow(index), 210L)
  expect_equal(nrow(archive), 32130L)
  expect_identical(as.integer(table(substr(index$period, 1L, 4L))),
    c(36L, 35L, 35L, 35L, 35L, 34L)
  )
  expect_false(any(index$period %in% c(
    "2015-02-D2", "2016-02-D1", "2017-01-D3", "2018-02-D2",
    "2019-02-D1", "2019-05-D2"
  )))
})

test_that("every retained ten-day release has only official source tuples", {
  archive <- readNBS:::.nbs_load_tenday_history_observations()
  product <- archive[archive$product_code != "summary", ]
  summary <- archive[archive$product_code == "summary", ]

  expect_true(all(table(product$release_period) == 150L))
  expect_true(all(table(summary$release_period) == 3L))
  expect_setequal(unique(product$measure), c("price_yuan", "change_yuan", "change_percent"))
  expect_setequal(unique(summary$measure), c(
    "increased_product_count", "decreased_product_count", "flat_product_count"
  ))
  expect_equal(anyDuplicated(paste(product$release_period, product$product_identity, product$measure)), 0L)
  expect_true(all(product$observation_period_start <= product$observation_period_end))
  expect_false(any(grepl("^[一二三四五六七八九十]+、", product$row_label)))
  rebar <- product[
    product$release_period == "2014-01-D1" &
      product$product_code == "tuple_8577231d0712cbec4154", , drop = FALSE
  ]
  expect_setequal(rebar$measure, c("price_yuan", "change_yuan", "change_percent"))
  expect_true(all(rebar$row_label == "螺纹钢"))
  expect_true(all(rebar$specification == "Φ16-25mm,HRB400"))
  expect_true(all(rebar$dimension_label_en == "Rebar (Φ16-25mm,HRB400)"))
})

test_that("ten-day history is wired into exact dimensions and coverage policy", {
  dimensions <- nbs_dataset_dimensions(
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES", "en"
  )
  history_products <- dimensions[
    dimensions$dimension == "product_code" &
      grepl("^tuple_", dimensions$value), , drop = FALSE
  ]
  tuples <- nbs_dataset_valid_tuples(
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
  )
  inceptions <- readNBS:::.nbs_load_tenday_tuple_inceptions()

  expect_equal(nrow(history_products), 59L)
  expect_equal(nrow(tuples), 330L)
  expect_equal(nrow(inceptions), 330L)
  expect_false(any(grepl("[一-龥]", history_products$label_en)))
  expect_setequal(tuples$dimension_key, inceptions$dimension_key)
})

test_that("ten-day raw captures and archive hashes verify offline", {
  index <- readNBS:::.nbs_load_tenday_history_release_index()
  hashes <- vapply(index$period, function(period) digest::digest(
    file = readNBS:::.nbs_extdata_path(file.path(
      "nbs-tenday-history", paste0(gsub("-", "", period), ".html")
    )), algo = "sha256", serialize = FALSE
  ), character(1L))
  expect_identical(unname(hashes), index$source_capture_sha256)
  expect_equal(length(unique(index$snapshot_sha256)), 1L)
})

test_that("portable discovery targets point to every retained official capture", {
  targets <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-tenday-history-targets.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  paths <- file.path(
    dirname(readNBS:::.nbs_extdata_path("nbs-tenday-history-targets.csv")),
    targets$source_capture_file
  )
  hashes <- vapply(paths, function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ), character(1L))
  expect_identical(targets$period, readNBS:::.nbs_tenday_history_expected_periods())
  expect_true(all(grepl("^https://www\\.stats\\.gov\\.cn/", targets$href)))
  expect_false(any(grepl("^/", targets$source_capture_file)))
  expect_identical(unname(hashes), targets$source_capture_sha256)
})
