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
#' `nbs_client()` creates the configuration and bounded, in-memory response
#' cache used by readNBS requests. Construction performs no network request,
#' does not create files, and does not require credentials. Each client owns
#' its own configuration and cache; clients do not share process-global HTTP
#' or cache state.
#'
#' The default portal bases are the official English and Chinese NBS public
#' data portals. They can be overridden with `READNBS_BASE_URL_EN` and
#' `READNBS_BASE_URL_ZH`. The observation endpoint list can similarly be
#' overridden with `READNBS_OBSERVATION_PATH`, as a comma-separated list. The
#' default transport uses `httr2` to request those URLs; the built-in bases are
#' HTTPS.
#'
#' @param base_url_en A length-one character string giving the base URL for
#'   English catalogue and observation requests. The default reads
#'   `READNBS_BASE_URL_EN`, falling back to the package's official English NBS
#'   endpoint. Trailing slashes are removed.
#' @param base_url_zh A length-one character string giving the base URL for
#'   Chinese catalogue and observation requests. The default reads
#'   `READNBS_BASE_URL_ZH`, falling back to the package's official Chinese NBS
#'   endpoint. Trailing slashes are removed.
#' @param observation_path A non-empty character vector of observation endpoint
#'   paths. The default reads `READNBS_OBSERVATION_PATH` as a comma-separated
#'   list and otherwise uses `"stream/esData"`. Paths are de-duplicated and
#'   tried in order only when the preceding path returns HTTP 404 or 405.
#' @param timeout A finite numeric per-attempt timeout in seconds, from 1 to
#'   60. The default is 15. This bound applies to each transport attempt, not
#'   to the complete operation including retries.
#' @param retries A non-negative integer retry count, from 0 to 5; the default
#'   is 2L. Transport errors and HTTP 429, 500, 502, 503, and 504 responses are
#'   retried with bounded exponential delays. A request can therefore have up
#'   to `retries + 1` attempts.
#' @param cache_max_entries A non-negative integer maximum number of cached
#'   observation responses for this client, from 0 to 512. The default is
#'   128L. Zero disables response caching.
#' @param cache_ttl A finite cache lifetime in seconds, from 0 to 86400. The
#'   default is 3600. Zero disables response caching. Expired entries are
#'   discarded when read; the cache is never written to disk.
#' @param max_periods A positive integer limit on the number of periods accepted
#'   by one logical series request, from 1 to 1200. The default is 600L. It is
#'   a guard against accidentally requesting an unexpectedly large range.
#' @param transport A function used for one HTTP attempt, or `NULL` to use the
#'   package transport. An injected function receives a request list containing
#'   `method`, `url`, `path`, `query`, `body`, `headers`, `timeout`,
#'   `operation`, and `attempt`; it must return a list containing `status`,
#'   `headers`, and `body`. `body` must be a raw vector or one character value.
#'   This hook is useful for deterministic tests and examples.
#' @param sleep A function used between retries; the default is [Sys.sleep]. It
#'   receives the delay in seconds. The function is never called when
#'   `retries = 0` or when no retry is needed.
#' @param now A function returning one non-missing `POSIXct` value; the default
#'   is `function() Sys.time()`. The clock controls request provenance and the
#'   default end period for ordinary series fetches.
#' @return An environment with class `nbs_client` (and no other class). It
#'   contains the validated bindings `base_url_en`, `base_url_zh`,
#'   `observation_path`, `timeout`, `retries`, `cache_max_entries`, `cache_ttl`,
#'   `max_periods`, `transport`, `sleep`, `now`, `cache`, and `cache_order`.
#'   The first eleven bindings describe request configuration and hooks; `cache`
#'   is the per-client environment used for response entries and `cache_order`
#'   is its character LRU order. Its configuration is used by the public fetch
#'   and discovery functions, and its cache is private to the returned
#'   environment. The object is also suitable for [print()].
#' @section Errors:
#' Invalid arguments, including out-of-range bounds or malformed injected
#' hooks, signal an `nbs_argument_error` condition. The constructor does not
#' contact the NBS service, so transport and HTTP conditions cannot occur until
#' a request function such as [nbs_fetch_series()] or [nbs_tree()] is called.
#' Those request functions can signal `nbs_transport_error` for a failed
#' attempt, `nbs_transport_contract_error` for an invalid injected response,
#' `nbs_http_error` for a non-success status, `nbs_endpoint_error` when every
#' configured observation path is unavailable, `nbs_waf_error` for an NBS
#' challenge page, `nbs_json_error` for invalid JSON, `nbs_api_error` for a
#' declared API failure, and route-specific schema or identity conditions.
#' @family client configuration
#' @seealso [nbs_clear_cache()], [nbs_fetch_series()], [nbs_tree()]
#' @examples
#' client <- nbs_client(timeout = 10, retries = 0L, cache_max_entries = 16L)
#' print(client)
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

#' Print an NBS client configuration
#'
#' The method prints the portal bases, configured observation paths, and the
#' request/cache bounds without performing a network request. It is intended
#' for a quick, human-readable configuration check; it does not print the
#' contents of the response cache.
#'
#' @param x An object returned by [nbs_client()].
#' @param ... Additional arguments accepted for consistency with the base
#'   `print()` generic. They are currently ignored.
#' @return `x`, invisibly.
#' @section Errors:
#' An invalid or modified client signals an `nbs_argument_error` condition.
#' @family client configuration
#' @seealso [nbs_client()]
#' @examples
#' print(nbs_client(base_url_en = "https://example.invalid/nbs"))
#' @method print nbs_client
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
