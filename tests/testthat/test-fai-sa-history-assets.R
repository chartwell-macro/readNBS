test_that("retained FAI SA history is a continuous official-vintage archive", {
  index <- utils::read.csv(readNBS:::.nbs_extdata_path("nbs-fai-sa-history-release-index.csv"), stringsAsFactors = FALSE)
  selected <- utils::read.csv(gzfile(readNBS:::.nbs_extdata_path("nbs-fai-sa-history-selected.csv.gz")), stringsAsFactors = FALSE)
  vintages <- utils::read.csv(gzfile(readNBS:::.nbs_extdata_path("nbs-fai-sa-history-vintages.csv.gz")), stringsAsFactors = FALSE)
  expect_identical(as.character(index$snapshot_id), as.character(2011:2025))
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
  expect_equal(nrow(vintages), 179L)
  expect_equal(nrow(selected), 172L)
  expect_identical(selected$observation_date, format(seq(as.Date("2011-02-01"), as.Date("2025-05-01"), by = "month")))
  expect_equal(anyDuplicated(selected$observation_date), 0L)
  expect_equal(index$selected_cells[[1L]], 11L)
  expect_equal(index$selected_cells[[15L]], 5L)
})

test_that("FAI SA inception and endpoint values match retained official tables", {
  selected <- utils::read.csv(gzfile(readNBS:::.nbs_extdata_path("nbs-fai-sa-history-selected.csv.gz")), stringsAsFactors = FALSE)
  rows <- selected[match(c("2011-02-01", "2011-03-01", "2012-01-01", "2020-02-01", "2025-05-01"), selected$observation_date), ]
  expect_equal(rows$value, c(0.18, 1.66, 1.24, -23.12, -0.77))
  expect_identical(as.character(rows$snapshot_id), c("2011", "2011", "2012", "2020", "2025"))
})

test_that("FAI SA helper preserves original annual revision provenance", {
  row <- readNBS:::.nbs_load_press_series_manifest()
  row <- row[row$series_id == "NBS/CN/M/FAI/TOTAL/MOM_PERCENT_OFFICIAL_SA", , drop = FALSE]
  result <- readNBS:::.nbs_fetch_fai_sa_history(row, as.Date("2011-02-01"), as.Date("2011-03-01"), "en")
  expect_equal(result$value, c(0.18, 1.66))
  expect_identical(result$source_vintage, rep("official_nbs_revision_snapshot_2012-01-17", 2L))
  expect_identical(result$route, rep("press_revision_table_archive", 2L))
})

test_that("FAI SA raw captures are hashed in a source checkout", {
  index <- utils::read.csv(
    readNBS:::.nbs_extdata_path("nbs-fai-sa-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  capture_dir <- file.path("inst", "extdata", "nbs-fai-sa-history")
  if (!dir.exists(capture_dir)) {
    capture_dir <- system.file(
      "extdata", "nbs-fai-sa-history", package = "readNBS", mustWork = FALSE
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
    # The CRAN tarball intentionally excludes repository-only captures; the
    # normalized selected archive remains covered by the tests above.
    expect_false(any(present))
  }
})
