test_that("retained industrial SA history is a continuous selected official-vintage archive", {
  index <- utils::read.csv(
    system.file("extdata", "nbs-industry-sa-history-release-index.csv", package = "readNBS"),
    stringsAsFactors = FALSE
  )
  selected <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-industry-sa-history-selected.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  vintages <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-industry-sa-history-vintages.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(index), 15L)
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
  expect_true(all(index$expected_vintage_cells %in% c(13L, 14L)))
  expect_equal(nrow(selected), 172L)
  expect_equal(selected$observation_date, format(seq(as.Date("2011-02-01"), as.Date("2025-05-01"), by = "month")))
  expect_equal(anyDuplicated(selected$observation_date), 0L)
  expect_true(all(selected$data_status == "official_revised_history_snapshot"))
  expect_true(all(grepl("^official_nbs_revision_snapshot_", selected$source_vintage)))
  expect_equal(sort(unique(selected$snapshot_id)), sort(index$snapshot_id))
  expect_true(nrow(vintages) > nrow(selected))
})

test_that("industrial SA inception and revision values match exact official NBS tables", {
  selected <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-industry-sa-history-selected.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  picked <- selected[match(c("2011-02-01", "2012-02-01", "2019-02-01", "2020-02-01", "2025-05-01"), selected$observation_date), ]
  expect_equal(picked$value, c(0.98, 0.61, 0.47, -22.10, 0.61))
  expect_equal(picked$snapshot_id, c("2012", "2013", "2020", "2021", "2025-06"))
})

test_that("industry SA archive helper retains source revision vintage", {
  row <- readNBS:::.nbs_load_press_series_manifest()
  row <- row[row$series_id == "NBS/CN/M/INDUSTRY/VALUE_ADDED/MOM_PERCENT_OFFICIAL_SA", , drop = FALSE]
  result <- readNBS:::.nbs_fetch_industry_sa_history(
    row, as.Date("2011-02-01"), as.Date("2011-03-01"), "en"
  )
  expect_equal(result$value, c(0.98, 1.08))
  expect_equal(result$source_vintage, rep("official_nbs_revision_snapshot_2012-03-09", 2L))
  expect_equal(result$route, rep("press_revision_table_archive", 2L))
})

test_that("industrial SA raw captures are hashed in a source checkout", {
  index <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-industry-sa-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  capture_dir <- file.path("inst", "extdata", "nbs-industry-sa-history")
  if (!dir.exists(capture_dir)) {
    capture_dir <- system.file(
      "extdata", "nbs-industry-sa-history", package = "readNBS", mustWork = FALSE
    )
  }
  paths <- file.path(capture_dir, paste0(index$snapshot_id, ".html"))
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
