#' Retrieve one level of the official NBS catalogue tree
#'
#' `nbs_tree()` performs one explicit live catalogue-discovery request for the
#' children of `parent_id`. It is useful when the bundled, offline manifest
#' used by [nbs_search()] does not yet contain a portal node. The root `code`
#' selects a portal frequency tree; use `parent_id = ""` for its root and a
#' returned `catalog_uuid` to descend one level at a time.
#'
#' The request uses the client's configured portal, timeout, retries, and
#' transport. It is not response-cache backed and normally requires network
#' access. Supplying an injected `transport` makes the operation deterministic
#' and is the recommended pattern for tests and examples.
#'
#' @param code A scalar NBS root code, usually `1` for monthly, `2` for
#'   quarterly, or `3` for annual data. It is converted to character for the
#'   portal query.
#' @param parent_id A character catalogue UUID identifying the parent node, or
#'   `""` (the default) to query the selected root. The value is passed to the
#'   official endpoint as `pid`.
#' @param language Portal language, either `"en"` (the default) or `"zh"`.
#' @param client An object returned by [nbs_client()].
#' @return A base `data.frame` (`stringsAsFactors = FALSE`) in the order
#'   returned by NBS. It has these columns: `catalog_uuid`, `parent_uuid`,
#'   `name`, `type`, `level`, `is_leaf`, `start_year`, `end_year`, and
#'   `explanation`. The first four and last three columns are character;
#'   `level` is integer and `is_leaf` is logical. Null portal fields are
#'   represented by `NA` in their character/logical column. The result carries
#'   three provenance attributes: `raw_sha256` (a lower-case hexadecimal
#'   SHA-256 hash of the response body), `retrieved_at` (one `POSIXct` value),
#'   and `source_url` (the requested URL). An empty valid response retains the
#'   same columns and attributes.
#' @section Conditions and side effects:
#' Invalid `code`, `parent_id`, or `client` values signal
#' `nbs_argument_error`; an unsupported `language` is reported by
#' [match.arg()]. Network and response
#' failures may signal `nbs_transport_error`, `nbs_http_error`, `nbs_waf_error`,
#' `nbs_json_error`, `nbs_api_error`, or `nbs_schema_error`; a malformed
#' injected transport can signal `nbs_transport_contract_error`. The call may
#' retry according to `client`, performs no filesystem write, and does not
#' alter the client's response cache.
#' @family discovery
#' @seealso [nbs_indicators()], [nbs_search()], [nbs_client()]
#' @examples
#' tree_file <- system.file(
#'   "extdata", "examples", "tree-monthly.json", package = "readNBS"
#' )
#' tree_transport <- function(request) {
#'   stopifnot(identical(request$path, "new/queryIndexTreeAsync"))
#'   list(
#'     status = 200L,
#'     headers = list(`content-type` = "application/json"),
#'     body = paste(readLines(tree_file, encoding = "UTF-8", warn = FALSE),
#'       collapse = "\n")
#'   )
#' }
#' tree_client <- nbs_client(
#'   transport = tree_transport,
#'   retries = 0L,
#'   now = function() as.POSIXct("2026-07-29 12:00:00", tz = "UTC")
#' )
#' tree <- nbs_tree(1, client = tree_client)
#' tree[c("catalog_uuid", "name", "is_leaf")]
#' @export
nbs_tree <- function(
    code,
    parent_id = "",
    language = c("en", "zh"),
    client = nbs_client()) {
  language <- match.arg(language)
  code <- .nbs_scalar_character(as.character(code), "code")
  parent_id <- .nbs_scalar_character(
    parent_id, "parent_id", allow_empty = TRUE
  )
  response <- .nbs_request_json(
    client = client,
    method = "GET",
    path = "new/queryIndexTreeAsync",
    query = list(pid = parent_id, code = code),
    language = language,
    operation = "NBS catalogue tree request"
  )
  .nbs_parse_tree(response)
}

#' Retrieve official indicator metadata for an NBS catalogue leaf
#'
#' `nbs_indicators()` performs one explicit live metadata request for a
#' catalogue leaf UUID. It exposes the portal's indicator identity, unit, time
#' range, and annotation fields before a caller constructs an observation
#' request. Normal offline search and bundled catalogue helpers do not call
#' this endpoint.
#'
#' The request uses the client's configured portal, timeout, retries, and
#' transport. It is not response-cache backed and normally requires network
#' access. An injected `transport` can serve a small JSON fixture for a
#' deterministic example or test.
#'
#' @param catalog_id A 32-character, lower-case hexadecimal NBS catalogue leaf
#'   UUID, such as the `catalog_uuid` returned by [nbs_tree()].
#' @param language Portal language, either `"en"` (the default) or `"zh"`.
#'   This controls the portal URL; the returned indicator fields retain the
#'   source labels supplied by NBS.
#' @param client An object returned by [nbs_client()].
#' @return A base `data.frame` (`stringsAsFactors = FALSE`) in the order of the
#'   NBS indicator list. Its character columns are `indicator_uuid`,
#'   `catalog_uuid`, `name`, `display_name`, `economic_key`,
#'   `period_dimension`, `unit_uuid`, `unit`, `start_date`, `end_date`,
#'   `annotation`, and `note`. Every returned `catalog_uuid` is validated to
#'   equal `catalog_id`. Dates and period dimensions are kept as character
#'   strings because they are portal metadata, not converted observations. The
#'   result carries `raw_sha256` (the response-body SHA-256), `retrieved_at` (a
#'   `POSIXct` value), and `source_url` attributes. An empty valid list retains
#'   the same columns and attributes.
#' @section Conditions and side effects:
#' An invalid UUID or client signals `nbs_argument_error`; an unsupported
#' `language` is reported by [match.arg()]. Network and
#' response failures may signal `nbs_transport_error`, `nbs_http_error`,
#' `nbs_waf_error`, `nbs_json_error`, `nbs_api_error`, or `nbs_schema_error`; an
#' indicator returned for a different catalogue signals
#' `nbs_catalogue_identity_error`. A malformed injected transport can signal
#' `nbs_transport_contract_error`. The call may retry according to `client`,
#' performs no filesystem write, and does not alter the client's response
#' cache.
#' @family discovery
#' @seealso [nbs_tree()], [nbs_search()], [nbs_client()]
#' @examples
#' indicators_file <- system.file(
#'   "extdata", "examples", "indicators-cpi-2026.json", package = "readNBS"
#' )
#' indicators_transport <- function(request) {
#'   stopifnot(identical(request$path, "new/queryIndicatorsByCid"))
#'   list(
#'     status = 200L,
#'     headers = list(`content-type` = "application/json"),
#'     body = paste(readLines(indicators_file, encoding = "UTF-8", warn = FALSE),
#'       collapse = "\n")
#'   )
#' }
#' indicators_client <- nbs_client(
#'   transport = indicators_transport,
#'   retries = 0L,
#'   now = function() as.POSIXct("2026-07-29 12:00:00", tz = "UTC")
#' )
#' indicators <- nbs_indicators(
#'   "5c7452825c7c4dcba391db5ca7f335c5",
#'   client = indicators_client
#' )
#' indicators[c("indicator_uuid", "display_name", "unit")]
#' @export
nbs_indicators <- function(
    catalog_id,
    language = c("en", "zh"),
    client = nbs_client()) {
  language <- match.arg(language)
  catalog_id <- .nbs_scalar_character(catalog_id, "catalog_id")
  if (!grepl("^[0-9a-f]{32}$", catalog_id)) {
    .nbs_abort(
      "nbs_argument_error",
      "`catalog_id` must be a 32-character lowercase hexadecimal UUID."
    )
  }
  response <- .nbs_request_json(
    client = client,
    method = "GET",
    path = "new/queryIndicatorsByCid",
    query = list(cid = catalog_id),
    language = language,
    operation = "NBS indicator metadata request"
  )
  .nbs_parse_indicators(response, catalog_id)
}
