test_that("transient failures retry only to the configured bound", {
  calls <- 0L
  transport <- function(request) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(list(
        status = 503L,
        headers = list(`content-type` = "application/json"),
        body = "{}"
      ))
    }
    fixture_response("cpi-2026-june.json")
  }
  client <- nbs_client(
    transport = transport,
    retries = 1L,
    sleep = no_sleep,
    now = fixed_now
  )
  result <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  expect_equal(result$value, 101)
  expect_equal(calls, 2L)

  failed_calls <- 0L
  failed <- nbs_client(
    transport = function(request) {
      failed_calls <<- failed_calls + 1L
      list(
        status = 503L,
        headers = list(`content-type` = "application/json"),
        body = "{}"
      )
    },
    retries = 2L,
    sleep = no_sleep,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-06", "2026-06",
      client = failed
    ),
    class = "nbs_http_error"
  )
  expect_equal(failed_calls, 3L)
})

test_that("404 and 405 trigger only configured endpoint fallback", {
  paths <- character()
  client <- nbs_client(
    observation_path = c("gone", "stream/esData"),
    retries = 0L,
    transport = function(request) {
      paths <<- c(paths, request$path)
      if (request$path == "gone") {
        return(list(
          status = 404L,
          headers = list(`content-type` = "application/json"),
          body = "{}"
        ))
      }
      fixture_response("cpi-2026-june.json")
    },
    now = fixed_now
  )
  result <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  expect_equal(result$value, 101)
  expect_equal(paths, c("gone", "stream/esData"))

  gone <- nbs_client(
    observation_path = c("gone-a", "gone-b"),
    retries = 0L,
    transport = function(request) {
      list(
        status = 405L,
        headers = list(`content-type` = "application/json"),
        body = "{}"
      )
    },
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-06", "2026-06",
      client = gone
    ),
    class = "nbs_endpoint_error"
  )
})

test_that("per-client cache is bounded, reused and explicitly clearable", {
  calls <- 0L
  client <- nbs_client(
    cache_max_entries = 1L,
    transport = function(request) {
      calls <<- calls + 1L
      fixture_response("cpi-2026-june.json")
    },
    retries = 0L,
    now = fixed_now
  )

  first <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  second <- nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  expect_equal(first, second)
  expect_equal(calls, 1L)
  expect_length(ls(client$cache, all.names = TRUE), 1L)

  nbs_clear_cache(client)
  expect_length(ls(client$cache, all.names = TRUE), 0L)
  nbs_fetch_series(
    "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
    "2026-06", "2026-06",
    client = client
  )
  expect_equal(calls, 2L)
})

test_that("API state failures and invalid JSON are distinguished", {
  api_client <- nbs_client(
    transport = function(request) fixture_response("api-error.json"),
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-06", "2026-06",
      client = api_client
    ),
    class = "nbs_api_error"
  )

  json_client <- nbs_client(
    transport = function(request) {
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = "{not-json"
      )
    },
    retries = 0L,
    now = fixed_now
  )
  expect_error(
    nbs_fetch_series(
      "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
      "2026-06", "2026-06",
      client = json_client
    ),
    class = "nbs_json_error"
  )
})

