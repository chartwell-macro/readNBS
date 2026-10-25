test_that("retained retail SA history is a continuous selected official-vintage archive", {
  index <- utils::read.csv(
    system.file("extdata", "nbs-retail-sa-history-release-index.csv", package = "readNBS"),
    stringsAsFactors = FALSE
  )
  selected <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-retail-sa-history-selected.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  vintages <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-retail-sa-history-vintages.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(index), 15L)
  expect_true(all(grepl("^[0-9a-f]{64}$", index$source_capture_sha256)))
  expect_equal(index$expected_vintage_cells, c(11L, rep(12L, 13L), 13L))
  expect_equal(nrow(selected), 172L)
  expect_equal(selected$observation_date, format(seq(as.Date("2011-02-01"), as.Date("2025-05-01"), by = "month")))
  expect_equal(anyDuplicated(selected$observation_date), 0L)
  expect_true(all(selected$data_status == "official_revised_history_snapshot"))
  expect_true(all(grepl("^official_nbs_revision_snapshot_", selected$source_vintage)))
  expect_equal(sort(unique(selected$snapshot_id)), sort(index$snapshot_id))
  expect_true(nrow(vintages) > nrow(selected))
})

test_that("retail SA inception and revision values match exact official NBS tables", {
  selected <- utils::read.csv(
    gzfile(system.file("extdata", "nbs-retail-sa-history-selected.csv.gz", package = "readNBS")),
    stringsAsFactors = FALSE
  )
  picked <- selected[match(c("2011-02-01", "2012-01-01", "2019-01-01", "2020-02-01", "2025-05-01"), selected$observation_date), ]
  expect_equal(picked$value, c(1.30, 0.19, 0.83, 0.95, 0.93))
  expect_equal(picked$snapshot_id, c("2011", "2012", "2019", "2020", "2025-06"))
})

test_that("retail SA archive helper retains source revision vintage", {
  row <- readNBS:::.nbs_load_press_series_manifest()
  row <- row[row$series_id == "NBS/CN/M/RETAIL/TOTAL/MOM_PERCENT_OFFICIAL_SA", , drop = FALSE]
  result <- readNBS:::.nbs_fetch_retail_sa_history(
    row, as.Date("2011-02-01"), as.Date("2011-03-01"), "en"
  )
  expect_equal(result$value, c(1.30, 1.35))
  expect_equal(result$source_vintage, rep("official_nbs_revision_snapshot_2012-01-17", 2L))
  expect_equal(result$route, rep("press_revision_table_archive", 2L))
})
