#' Retrieve one level of the official NBS catalogue tree
#'
#' This is an explicit live discovery operation. Normal catalogue search uses
#' [nbs_search()] and performs no network request.
#'
#' @param code NBS root code, such as `1` monthly, `2` quarterly or `3` annual.
#' @param parent_id Optional parent catalogue UUID. Use `""` for the root.
#' @param language Portal language, either `"en"` or `"zh"`.
#' @param client An object returned by [nbs_client()].
#' @return A data frame with response provenance in attributes.
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
#' This is an explicit live metadata operation. Normal search uses the bundled
#' manifest and performs no network request.
#'
#' @param catalog_id NBS catalogue leaf UUID.
#' @param language Portal language, either `"en"` or `"zh"`.
#' @param client An object returned by [nbs_client()].
#' @return A data frame with response provenance in attributes.
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

