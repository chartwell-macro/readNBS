.nbs_default_base_en <- paste0(
  "https://data.stats.gov.cn/dg/website/",
  "publicrelease/en/web/external"
)

.nbs_default_base_zh <- paste0(
  "https://data.stats.gov.cn/dg/website/",
  "publicrelease/web/external"
)

.nbs_default_transport <- function(request) {
  req <- httr2::request(request$url)
  if (length(request$query)) {
    req <- do.call(
      httr2::req_url_query,
      c(list(req), request$query)
    )
  }
  req <- do.call(
    httr2::req_headers,
    c(list(req), request$headers)
  )
  req <- httr2::req_timeout(req, request$timeout)
  req <- httr2::req_error(req, is_error = function(response) FALSE)

  if (identical(request$method, "POST")) {
    json_body <- jsonlite::toJSON(
      request$body,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )
    req <- httr2::req_body_raw(
      req,
      charToRaw(enc2utf8(json_body)),
      type = "application/json"
    )
  }

  response <- httr2::req_perform(req)
  list(
    status = httr2::resp_status(response),
    headers = httr2::resp_headers(response),
    body = httr2::resp_body_raw(response)
  )
}

#' Create an NBS client
#'
#' Client construction performs no network request. Cache and configuration
#' are held per client, with no process-global session state.
#'
#' @param base_url_en Base URL for the English public portal JSON flow.
#' @param base_url_zh Base URL for the Chinese public portal JSON flow.
#' @param observation_path One or more observation endpoint paths, tried in
#'   order only when a preceding path returns HTTP 404 or 405.
#' @param timeout Per-attempt timeout in seconds, from 1 to 60.
#' @param retries Retry count for transport failures and transient HTTP
#'   statuses, from 0 to 5.
#' @param cache_max_entries Maximum per-client cached queries, from 0 to 512.
#' @param cache_ttl Cache lifetime in seconds, from 0 to 86400.
#' @param max_periods Maximum periods accepted by one logical fetch, from 1 to
#'   1200.
#' @param transport Optional injected transport function. It receives a request
#'   list and must return `list(status, headers, body)`.
#' @param sleep Optional injected sleep function used between retries.
#' @param now Optional clock function returning a `POSIXct` value.
#' @return An `nbs_client` environment.
#' @export
nbs_client <- function(
    base_url_en = Sys.getenv("READNBS_BASE_URL_EN", .nbs_default_base_en),
    base_url_zh = Sys.getenv("READNBS_BASE_URL_ZH", .nbs_default_base_zh),
    observation_path = strsplit(
      Sys.getenv("READNBS_OBSERVATION_PATH", "stream/esData"),
      ",",
      fixed = TRUE
    )[[1L]],
    timeout = 15,
    retries = 2L,
    cache_max_entries = 128L,
    cache_ttl = 3600,
    max_periods = 600L,
    transport = NULL,
    sleep = Sys.sleep,
    now = function() Sys.time()) {
  base_url_en <- .nbs_scalar_character(base_url_en, "base_url_en")
  base_url_zh <- .nbs_scalar_character(base_url_zh, "base_url_zh")
  if (!is.character(observation_path) || !length(observation_path) ||
      anyNA(observation_path)) {
    .nbs_abort(
      "nbs_argument_error",
      "`observation_path` must contain at least one non-missing path."
    )
  }
  observation_path <- trimws(observation_path)
  if (any(!nzchar(observation_path))) {
    .nbs_abort(
      "nbs_argument_error",
      "`observation_path` must not contain an empty path."
    )
  }
  timeout <- .nbs_scalar_number(timeout, "timeout", 1, 60)
  retries <- .nbs_scalar_number(retries, "retries", 0, 5, integer = TRUE)
  cache_max_entries <- .nbs_scalar_number(
    cache_max_entries, "cache_max_entries", 0, 512, integer = TRUE
  )
  cache_ttl <- .nbs_scalar_number(cache_ttl, "cache_ttl", 0, 86400)
  max_periods <- .nbs_scalar_number(
    max_periods, "max_periods", 1, 1200, integer = TRUE
  )
  transport <- transport %||% .nbs_default_transport
  if (!is.function(transport)) {
    .nbs_abort("nbs_argument_error", "`transport` must be a function.")
  }
  if (!is.function(sleep)) {
    .nbs_abort("nbs_argument_error", "`sleep` must be a function.")
  }
  if (!is.function(now)) {
    .nbs_abort("nbs_argument_error", "`now` must be a function.")
  }

  client <- new.env(parent = emptyenv())
  client$base_url_en <- sub("/+$", "", base_url_en)
  client$base_url_zh <- sub("/+$", "", base_url_zh)
  client$observation_path <- unique(observation_path)
  client$timeout <- timeout
  client$retries <- retries
  client$cache_max_entries <- cache_max_entries
  client$cache_ttl <- cache_ttl
  client$max_periods <- max_periods
  client$transport <- transport
  client$sleep <- sleep
  client$now <- now
  client$cache <- new.env(parent = emptyenv(), hash = TRUE)
  client$cache_order <- character()
  class(client) <- "nbs_client"
  .nbs_validate_client(client)
}

.nbs_validate_client <- function(client) {
  if (!inherits(client, "nbs_client") || !is.environment(client)) {
    .nbs_abort(
      "nbs_argument_error",
      "`client` must be an object returned by `nbs_client()`."
    )
  }
  required <- c(
    "base_url_en", "base_url_zh", "observation_path", "timeout",
    "retries", "cache_max_entries", "cache_ttl", "max_periods",
    "transport", "sleep", "now", "cache", "cache_order"
  )
  if (any(!vapply(required, exists, logical(1),
    envir = client, inherits = FALSE
  ))) {
    .nbs_abort(
      "nbs_argument_error",
      "The NBS client is incomplete or has been modified."
    )
  }
  now_value <- client$now()
  if (!inherits(now_value, "POSIXt") || length(now_value) != 1L ||
      is.na(now_value)) {
    .nbs_abort(
      "nbs_argument_error",
      "The client clock must return one non-missing POSIXct value."
    )
  }
  client
}

#' @export
print.nbs_client <- function(x, ...) {
  .nbs_validate_client(x)
  cat("<nbs_client>\n", sep = "")
  cat("  English base: ", x$base_url_en, "\n", sep = "")
  cat("  Chinese base: ", x$base_url_zh, "\n", sep = "")
  cat(
    "  Observation path: ",
    paste(x$observation_path, collapse = ", "),
    "\n",
    sep = ""
  )
  cat(
    "  Bounds: timeout=", x$timeout,
    "s, retries=", x$retries,
    ", cache=", x$cache_max_entries,
    ", max periods=", x$max_periods,
    "\n",
    sep = ""
  )
  invisible(x)
}

