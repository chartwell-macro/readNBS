fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

fixture_body <- function(name) {
  paste(readLines(fixture_path(name), warn = FALSE, encoding = "UTF-8"),
        collapse = "\n")
}

fixture_response <- function(name, status = 200L, content_type = "application/json") {
  list(
    status = status,
    headers = list(`content-type` = content_type),
    body = fixture_body(name)
  )
}

fixed_now <- function() {
  as.POSIXct("2026-07-29 12:00:00", tz = "UTC")
}

no_sleep <- function(seconds) {
  invisible(seconds)
}

make_observation_transport <- function(mapping, calls = NULL) {
  force(mapping)
  force(calls)
  function(request) {
    if (!is.null(calls)) {
      calls$n <- calls$n + 1L
      calls$requests[[length(calls$requests) + 1L]] <- request
    }
    key <- paste(
      request$body$cid,
      request$body$dts[[1L]],
      sep = "::"
    )
    fixture <- mapping[[key]]
    if (is.null(fixture)) {
      stop("No fixture mapped for ", key)
    }
    fixture_response(fixture)
  }
}

make_document_client <- function(name, calls = NULL) {
  client <- nbs_client(
    transport = function(request) {
      if (!is.null(calls)) {
        calls$n <- calls$n + 1L
        calls$requests[[length(calls$requests) + 1L]] <- request
      }
      fixture_response(name, content_type = "text/html; charset=UTF-8")
    },
    retries = 0L,
    now = fixed_now
  )
  client$press_fixture_mode <- TRUE
  client
}
