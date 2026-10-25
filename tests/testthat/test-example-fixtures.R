example_fixture_names <- c(
  "cpi-2026-june.json",
  "tree-monthly.json",
  "indicators-cpi-2026.json",
  "yearbook-2011-left.html",
  "yearbook-table-merged.html"
)

example_fixture_path <- function(name) {
  path <- system.file("extdata", "examples", name, package = "readNBS")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata", "examples", name)
  }
  if (!file.exists(path)) {
    stop("Example fixture is not installed or available in the source tree: ", name)
  }
  path
}

example_fixture_raw <- function(name) {
  path <- example_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

example_fixture_response <- function(name, content_type) {
  list(
    status = 200L,
    headers = list(`content-type` = content_type),
    body = example_fixture_raw(name)
  )
}

test_that("installed example fixtures preserve the test payload bytes", {
  paths <- vapply(example_fixture_names, example_fixture_path, character(1L))
  source_paths <- testthat::test_path("fixtures", example_fixture_names)
  sizes <- file.info(paths)$size

  expect_true(all(file.exists(paths)))
  expect_true(all(sizes > 0))
  expect_lte(sum(sizes), 10000)
  expect_lte(max(sizes), 2000)

  for (index in seq_along(paths)) {
    installed <- readBin(paths[[index]], what = "raw", n = sizes[[index]])
    source <- readBin(
      source_paths[[index]], what = "raw",
      n = file.info(source_paths[[index]])$size
    )
    # The JSON test fixtures carry one redundant empty line at EOF.  Compare
    # the complete payload bytes while ignoring only that source-only newline.
    source_payload <- if (
      length(source) >= 2L &&
        identical(source[[length(source)]], as.raw(10L)) &&
        identical(source[[length(source) - 1L]], as.raw(10L))
    ) {
      source[-length(source)]
    } else {
      source
    }
    expect_identical(installed, source_payload, info = example_fixture_names[[index]])
    expect_identical(
      digest::digest(installed, algo = "sha256", serialize = FALSE),
      digest::digest(source_payload, algo = "sha256", serialize = FALSE),
      info = example_fixture_names[[index]]
    )
  }
})

test_that("JSON examples parse through public functions without network", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  client <- nbs_client(
    transport = function(request) {
      calls$n <- calls$n + 1L
      if (identical(request$path, "stream/esData")) {
        return(example_fixture_response("cpi-2026-june.json", "application/json"))
      }
      if (identical(request$path, "new/queryIndexTreeAsync")) {
        return(example_fixture_response("tree-monthly.json", "application/json"))
      }
      if (identical(request$path, "new/queryIndicatorsByCid")) {
        return(example_fixture_response("indicators-cpi-2026.json", "application/json"))
      }
      stop("Unexpected example-fixture request path: ", request$path)
    },
    retries = 0L,
    now = fixed_now
  )

  cpi <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    start = "2026-06", end = "2026-06", client = client
  )
  tree <- nbs_tree(1, client = client)
  indicators <- nbs_indicators(
    "5c7452825c7c4dcba391db5ca7f335c5", client = client
  )

  expect_equal(cpi$value, 101)
  expect_equal(tree$catalog_uuid, "fc982599aa684be7969d7b90b1bd0e84")
  expect_equal(indicators$indicator_uuid, "53180dfb9c14411ba4b762307c85920c")
  expect_equal(calls$n, 3L)
})

test_that("Yearbook examples parse through public functions without network", {
  index <- paste(
    readLines(example_fixture_path("yearbook-2011-left.html"),
      warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  discovered <- nbs_yearbook_discover(index, 2011L)
  routes <- nbs_yearbook_routes(index, 2011L)
  route <- routes[routes$source_table == "3-1", , drop = FALSE]
  route$expected_columns <- 3L
  route$expected_min_rows <- 5L
  route$header_rows <- 3L
  table_html <- paste(
    readLines(example_fixture_path("yearbook-table-merged.html"),
      warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  parsed <- nbs_yearbook_parse_table(
    table_html, route,
    retrieved_at = as.POSIXct("2026-08-11 00:00:00", tz = "UTC")
  )

  expect_equal(nrow(discovered), 6L)
  expect_equal(nrow(parsed), 4L)
  expect_identical(parsed$reference_year, c(2009L, 2009L, 2010L, 2010L))
  expect_identical(unique(parsed$schema_era), "annual_population_census_revised")

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  client <- nbs_client(
    transport = function(request) {
      calls$n <- calls$n + 1L
      expect_identical(request$url, "https://www.stats.gov.cn/sj/ndsj/2011/left.htm")
      example_fixture_response("yearbook-2011-left.html", "text/html")
    },
    retries = 0L,
    now = fixed_now
  )
  live <- nbs_yearbook_discover_live(2011L, client)
  expect_equal(nrow(live), 6L)
  expect_equal(calls$n, 1L)
})
