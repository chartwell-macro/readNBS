test_that("NBS period encodings use the current MM SS and YY contract", {
  expect_equal(
    readNBS:::.nbs_encode_range(
      as.Date("2026-01-01"), as.Date("2026-06-01"), "M"
    ),
    "202601MM-202606MM"
  )
  expect_equal(
    readNBS:::.nbs_encode_range(
      as.Date("2026-01-01"), as.Date("2026-04-01"), "Q"
    ),
    "202601SS-202602SS"
  )
  expect_equal(
    readNBS:::.nbs_encode_range(
      as.Date("2020-01-01"), as.Date("2025-01-01"), "A"
    ),
    "2020YY-2025YY"
  )
})

test_that("period codes decode to stable labels and period-start dates", {
  month <- readNBS:::.nbs_decode_period("202606MM", "M")
  quarter <- readNBS:::.nbs_decode_period("202602SS", "Q")
  annual <- readNBS:::.nbs_decode_period("2025YY", "A")

  expect_equal(month$date, as.Date("2026-06-01"))
  expect_equal(month$period, "2026-06")
  expect_equal(quarter$date, as.Date("2026-04-01"))
  expect_equal(quarter$period, "2026-Q2")
  expect_equal(annual$date, as.Date("2025-01-01"))
  expect_equal(annual$period, "2025")
  expect_error(
    readNBS:::.nbs_decode_period("202602QQ", "Q"),
    class = "nbs_period_schema_error"
  )
})

