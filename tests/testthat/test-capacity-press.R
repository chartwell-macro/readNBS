parse_capacity_fixture <- function(name, release = NULL) {
  document <- xml2::read_html(fixture_body(name))
  tables <- rvest::html_table(document, fill = TRUE, header = FALSE)
  readNBS:::.nbs_parse_capacity_utilisation_panel(
    tables, document, release = release
  )
}

test_that("capacity parser normalizes the four-column detailed era", {
  result <- parse_capacity_fixture("press-capacity-2018-q2.html")

  expect_equal(nrow(result), 19L * 4L)
  expect_equal(anyDuplicated(paste(result$industry, result$measure)), 0L)
  expect_equal(anyDuplicated(result$series_id), 0L)
  expect_true(all(grepl(
    "^NBS/CN/Q/CAPACITY_UTILISATION/[A-Z_]+/[A-Z_]+$",
    result$series_id
  )))
  expect_setequal(
    unique(result$industry),
    names(readNBS:::.nbs_capacity_industry_aliases())
  )
  expect_setequal(
    unique(result$measure),
    c(
      "current_percent", "current_yoy_change_pp",
      "ytd_percent", "ytd_yoy_change_pp"
    )
  )
  expect_setequal(unique(result$period_basis), c("current", "ytd"))
  expect_setequal(unique(result$unit), c("Percent", "Percentage points"))
  expect_true(all(is.na(result$observation_date)))
  expect_true(all(result$reported_observation_date == as.Date("2018-04-01")))
  expect_true(all(result$observation_period_end == as.Date("2018-06-30")))
  compared <- grepl("yoy_change_pp$", result$measure)
  expect_true(all(
    result$comparison_period_end[compared] == as.Date("2017-06-30")
  ))
  expect_true(all(is.na(result$comparison_period_end[!compared])))
  expect_true(all(
    result$observation_period_start[result$period_basis == "current"] ==
      as.Date("2018-04-01")
  ))
  expect_true(all(
    result$observation_period_start[result$period_basis == "ytd"] ==
      as.Date("2018-01-01")
  ))
  expect_setequal(
    unique(result$published_value_type),
    c(
      "published_utilisation_rate",
      "published_percentage_point_change"
    )
  )
  expect_equal(unique(result$schema_version),
    "capacity-gbt4754-2017-detailed-v1"
  )
  expect_equal(unique(result$classification_id), "GB/T4754-2017")
  expect_equal(unique(result$coverage_version),
    "approximately-90000-enterprises"
  )

  total <- result[result$industry == "total", ]
  expect_equal(total$value[total$measure == "current_percent"], 76.8)
  expect_equal(total$value[total$measure == "current_yoy_change_pp"], 0)
  expect_equal(total$value[total$measure == "ytd_percent"], 76.7)
  expect_equal(total$value[total$measure == "ytd_yoy_change_pp"], 0.3)
})

test_that("capacity Q1 tables do not invent duplicate YTD measures", {
  result <- parse_capacity_fixture("press-capacity-2021-q1.html")

  expect_equal(nrow(result), 19L * 2L)
  expect_setequal(
    unique(result$measure), c("current_percent", "current_yoy_change_pp")
  )
  expect_setequal(unique(result$period_basis), "current")
  expect_true(all(grepl("/CURRENT_", result$series_id, fixed = TRUE)))
  expect_true(all(result$reported_observation_date == as.Date("2021-01-01")))
  expect_true(all(result$observation_period_start == as.Date("2021-01-01")))
  expect_true(all(result$observation_period_end == as.Date("2021-03-31")))
  compared <- result$measure == "current_yoy_change_pp"
  expect_true(all(
    result$comparison_period_start[compared] == as.Date("2020-01-01")
  ))
  expect_true(all(
    result$comparison_period_end[compared] == as.Date("2020-03-31")
  ))
  expect_true(all(is.na(result$comparison_period_start[!compared])))
})

test_that("capacity release metadata takes precedence over page wording", {
  release <- data.frame(
    period = "2022-Q3",
    reference_period_end = "2022-09-30",
    stringsAsFactors = FALSE
  )
  result <- parse_capacity_fixture("press-capacity-2021-q1.html", release)

  expect_true(all(result$reported_observation_date == as.Date("2022-07-01")))
  expect_equal(unique(result$coverage_version),
    "approximately-110000-enterprises"
  )
})

test_that("capacity parser rejects incomplete category panels", {
  document <- xml2::read_html(fixture_body("press-capacity-2021-q1.html"))
  tables <- rvest::html_table(document, fill = TRUE, header = FALSE)
  tables[[1L]] <- tables[[1L]][-nrow(tables[[1L]]), , drop = FALSE]

  expect_error(
    readNBS:::.nbs_parse_capacity_utilisation_panel(tables, document),
    class = "nbs_press_schema_error"
  )
})

test_that("capacity parser collapses the one exact responsive-table duplicate", {
  document <- xml2::read_html(fixture_body("press-capacity-2018-q2.html"))
  tables <- rvest::html_table(document, fill = TRUE, header = FALSE)
  table_index <- readNBS:::.nbs_capacity_candidate_table(tables)$table_index
  table <- tables[[table_index]]
  tables[[table_index]] <- table[
    , c(1L, 2L, 3L, 4L, 4L, 5L), drop = FALSE
  ]

  parsed <- readNBS:::.nbs_parse_capacity_utilisation_panel(tables, document)

  expect_equal(nrow(parsed), 76L)
  expect_setequal(unique(parsed$measure), c(
    "current_percent", "current_yoy_change_pp",
    "ytd_percent", "ytd_yoy_change_pp"
  ))
})
