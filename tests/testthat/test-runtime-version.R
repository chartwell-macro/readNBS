test_that("runtime provenance follows the package version", {
  description <- as.character(utils::packageVersion("readNBS"))

  expect_identical(description, "0.8.3")
  expect_identical(readNBS:::.nbs_package_version(), description)

  source_dir <- testthat::test_path("..", "..", "R")
  if (dir.exists(source_dir)) {
    sources <- list.files(
      source_dir,
      pattern = "\\.[Rr]$",
      full.names = TRUE
    )
    expect_gt(length(sources), 0L)
    source_text <- unlist(
      lapply(sources, readLines, warn = FALSE),
      use.names = FALSE
    )
    expect_false(any(grepl("0.6.1", source_text, fixed = TRUE)))
  }
})

test_that("HTTP requests identify the current package version", {
  requests <- list()
  client <- nbs_client(
    retries = 0L,
    transport = function(request) {
      requests[[length(requests) + 1L]] <<- request
      if (identical(request$method, "GET")) {
        return(list(
          status = 200L,
          headers = list(`content-type` = "text/html"),
          body = "<html><body>ok</body></html>"
        ))
      }
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = '{"success":true,"state":20000,"data":[]}'
      )
    }
  )

  readNBS:::.nbs_request_json(
    client, "POST", "/query", body = list(), operation = "version test"
  )
  readNBS:::.nbs_request_document(client, "https://example.test/release")

  expected <- paste0("readNBS/", readNBS:::.nbs_package_version())
  expect_identical(
    vapply(requests, function(request) request$headers[["User-Agent"]], ""),
    rep(expected, 2L)
  )
})
