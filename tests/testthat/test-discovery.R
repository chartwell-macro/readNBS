test_that("tree and indicator metadata parse from offline fixtures", {
  transport <- function(request) {
    if (identical(request$path, "new/queryIndexTreeAsync")) {
      expect_equal(request$query, list(pid = "", code = "1"))
      return(fixture_response("tree-monthly.json"))
    }
    if (identical(request$path, "new/queryIndicatorsByCid")) {
      expect_equal(
        request$query$cid,
        "5c7452825c7c4dcba391db5ca7f335c5"
      )
      return(fixture_response("indicators-cpi-2026.json"))
    }
    stop("Unexpected request")
  }
  client <- nbs_client(
    transport = transport,
    retries = 0L,
    now = fixed_now
  )

  tree <- nbs_tree(1, client = client)
  expect_equal(tree$catalog_uuid, "fc982599aa684be7969d7b90b1bd0e84")
  expect_false(tree$is_leaf)
  expect_equal(nchar(attr(tree, "raw_sha256")), 64L)
  expect_s3_class(attr(tree, "retrieved_at"), "POSIXct")

  indicators <- nbs_indicators(
    "5c7452825c7c4dcba391db5ca7f335c5",
    client = client
  )
  expect_equal(
    indicators$indicator_uuid,
    "53180dfb9c14411ba4b762307c85920c"
  )
  expect_match(indicators$display_name, "Consumer Price Index")
  expect_equal(indicators$start_date, "2026-01-01 00:00:00")
  expect_equal(nchar(attr(indicators, "raw_sha256")), 64L)
})

test_that("HTML and malformed discovery schemas fail explicitly", {
  html_client <- nbs_client(
    transport = function(request) {
      list(
        status = 200L,
        headers = list(`content-type` = "text/html"),
        body = "<html>challenge</html>"
      )
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(nbs_tree(1, client = html_client), class = "nbs_waf_error")

  malformed_client <- nbs_client(
    transport = function(request) {
      fixture_response("schema-missing-values.json")
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_tree(1, client = malformed_client),
    class = "nbs_schema_error"
  )
})

