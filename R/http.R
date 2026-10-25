.nbs_base_url <- function(client, language) {
  if (identical(language, "en")) client$base_url_en else client$base_url_zh
}

.nbs_build_url <- function(base_url, path) {
  if (grepl("^https?://", path, ignore.case = TRUE)) {
    return(path)
  }
  paste0(sub("/+$", "", base_url), "/", sub("^/+", "", path))
}

.nbs_request_json <- function(
    client,
    method,
    path,
    query = list(),
    body = NULL,
    language = "en",
    operation = "NBS request") {
  .nbs_validate_client(client)
  language <- match.arg(language, c("en", "zh"))
  url <- .nbs_build_url(.nbs_base_url(client, language), path)
  headers <- list(
    Accept = "application/json, text/plain, */*",
    Referer = "https://data.stats.gov.cn/dg/website/page.html",
    `User-Agent` = paste0("readNBS/", .nbs_package_version())
  )
  request <- list(
    method = method,
    url = url,
    path = path,
    query = query,
    body = body,
    headers = headers,
    timeout = client$timeout,
    operation = operation
  )

  response <- NULL
  for (attempt in seq_len(client$retries + 1L)) {
    request$attempt <- attempt
    response <- tryCatch(
      client$transport(request),
      error = function(error) error
    )
    if (inherits(response, "error")) {
      if (attempt <= client$retries) {
        client$sleep(min(2^(attempt - 1L), 4))
        next
      }
      .nbs_abort(
        "nbs_transport_error",
        sprintf(
          "%s failed after %d attempt%s: %s",
          operation,
          attempt,
          if (attempt == 1L) "" else "s",
          conditionMessage(response)
        ),
        url = url,
        attempts = attempt,
        parent = response
      )
    }

    if (!is.list(response) || is.null(response$status) ||
        is.null(response$body)) {
      .nbs_abort(
        "nbs_transport_contract_error",
        "The transport must return `list(status, headers, body)`.",
        url = url
      )
    }
    status <- suppressWarnings(as.integer(response$status[[1L]]))
    if (length(status) != 1L || is.na(status) ||
        status < 100L || status > 599L) {
      .nbs_abort(
        "nbs_transport_contract_error",
        "The transport returned an invalid HTTP status.",
        url = url
      )
    }
    transient <- status %in% c(429L, 500L, 502L, 503L, 504L)
    if (transient && attempt <= client$retries) {
      client$sleep(min(2^(attempt - 1L), 4))
      next
    }
    break
  }

  raw_body <- .nbs_raw_body(response$body)
  status <- as.integer(response$status[[1L]])
  if (status < 200L || status >= 300L) {
    .nbs_abort(
      "nbs_http_error",
      sprintf("%s returned HTTP %d.", operation, status),
      status = status,
      url = url,
      raw_sha256 = .nbs_sha256(raw_body)
    )
  }

  body_text <- enc2utf8(rawToChar(raw_body))
  content_type <- .nbs_header(response$headers %||% list(), "content-type")
  if ((!is.na(content_type) &&
       grepl("text/html", content_type, fixed = TRUE)) ||
      grepl("^\\s*<", body_text)) {
    .nbs_abort(
      "nbs_waf_error",
      sprintf("%s returned HTML instead of JSON.", operation),
      url = url,
      raw_sha256 = .nbs_sha256(raw_body)
    )
  }

  payload <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(error) error
  )
  if (inherits(payload, "error") || !is.list(payload)) {
    .nbs_abort(
      "nbs_json_error",
      sprintf("%s returned invalid JSON.", operation),
      url = url,
      raw_sha256 = .nbs_sha256(raw_body),
      parent = if (inherits(payload, "error")) payload else NULL
    )
  }
  state <- suppressWarnings(as.integer(payload$state %||% NA_integer_))
  if (!isTRUE(payload$success) || length(state) != 1L ||
      is.na(state) || state != 20000L) {
    message <- .nbs_as_character_scalar(payload$message)
    .nbs_abort(
      "nbs_api_error",
      sprintf(
        "%s returned API state `%s`%s.",
        operation,
        if (is.na(state)) "missing" else state,
        if (is.na(message) || !nzchar(message)) "" else paste0(": ", message)
      ),
      state = state,
      api_message = message,
      url = url,
      raw_sha256 = .nbs_sha256(raw_body)
    )
  }

  list(
    payload = payload,
    raw_sha256 = .nbs_sha256(raw_body),
    url = url,
    status = status,
    headers = response$headers %||% list(),
    retrieved_at = as.POSIXct(client$now(), tz = "UTC")
  )
}

.nbs_observation_request <- function(client, body, language) {
  endpoint_errors <- list()
  for (path in client$observation_path) {
    result <- tryCatch(
      .nbs_request_json(
        client = client,
        method = "POST",
        path = path,
        body = body,
        language = language,
        operation = "NBS observation request"
      ),
      nbs_http_error = function(error) error
    )
    if (!inherits(result, "nbs_http_error")) {
      return(result)
    }
    endpoint_errors[[path]] <- result
    if (!result$status %in% c(404L, 405L)) {
      stop(result)
    }
  }
  .nbs_abort(
    "nbs_endpoint_error",
    sprintf(
      "All configured NBS observation endpoints returned HTTP 404 or 405: %s.",
      paste(client$observation_path, collapse = ", ")
    ),
    paths = client$observation_path,
    errors = endpoint_errors
  )
}

.nbs_request_document <- function(client, url, operation = "NBS document") {
  .nbs_validate_client(client)
  request <- list(
    method = "GET",
    url = .nbs_scalar_character(url, "url"),
    path = url,
    query = list(),
    body = NULL,
    headers = list(
      Accept = "text/html,application/xhtml+xml",
      `User-Agent` = paste0("readNBS/", .nbs_package_version())
    ),
    timeout = client$timeout,
    operation = operation
  )
  response <- NULL
  for (attempt in seq_len(client$retries + 1L)) {
    request$attempt <- attempt
    response <- tryCatch(client$transport(request), error = function(error) error)
    if (inherits(response, "error")) {
      if (attempt <= client$retries) {
        client$sleep(min(2^(attempt - 1L), 4))
        next
      }
      .nbs_abort(
        "nbs_transport_error",
        sprintf("%s failed after %d attempt(s).", operation, attempt),
        url = url,
        attempts = attempt,
        parent = response
      )
    }
    status <- suppressWarnings(as.integer(response$status[[1L]]))
    if (status %in% c(429L, 500L, 502L, 503L, 504L) &&
        attempt <= client$retries) {
      client$sleep(min(2^(attempt - 1L), 4))
      next
    }
    break
  }
  if (!is.list(response) || is.null(response$status) ||
      is.null(response$body)) {
    .nbs_abort(
      "nbs_transport_contract_error",
      "The transport must return `list(status, headers, body)`.",
      url = url
    )
  }
  status <- suppressWarnings(as.integer(response$status[[1L]]))
  raw_body <- .nbs_raw_body(response$body)
  if (length(status) != 1L || is.na(status) ||
      status < 200L || status >= 300L) {
    .nbs_abort(
      "nbs_http_error",
      sprintf("%s returned HTTP %s.", operation, status),
      status = status,
      url = url,
      raw_sha256 = .nbs_sha256(raw_body)
    )
  }
  body_text <- enc2utf8(rawToChar(raw_body))
  if (!grepl("<(?:html|body|table)\\b", body_text, ignore.case = TRUE)) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf("%s returned a non-HTML body.", operation),
      url = url,
      raw_sha256 = .nbs_sha256(raw_body)
    )
  }
  list(
    body_text = body_text,
    raw_sha256 = .nbs_sha256(raw_body),
    url = url,
    status = status,
    headers = response$headers %||% list(),
    retrieved_at = as.POSIXct(client$now(), tz = "UTC")
  )
}
