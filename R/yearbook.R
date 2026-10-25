# Yearbook acquisition is deliberately separate from the portal/press readers.
# It is a maintenance path: callers fetch and freeze an official HTML document,
# then normal parsing works only from that local document.

.nbs_yearbook_index_url <- function(yearbook) {
  yearbook <- .nbs_scalar_number(yearbook, "yearbook", 1949, 2100, integer = TRUE)
  sprintf("https://www.stats.gov.cn/sj/ndsj/%d/left.htm", yearbook)
}

.nbs_yearbook_clean_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub(
    paste0("[", "\r\n\t", intToUtf8(160L), intToUtf8(12288L), "]+"),
    " ", x, perl = TRUE
  )
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
}

# Yearbook pages retained by NBS before the modern portal are GB18030.  The
# byte-level snapshot manifest records the encoding so a future page cannot be
# silently parsed using the process locale.
.nbs_yearbook_decode_html <- function(raw_html, encoding = NULL) {
  if (is.raw(raw_html)) {
    raw_html <- rawToChar(raw_html)
  }
  if (!is.character(raw_html) || length(raw_html) != 1L || is.na(raw_html)) {
    .nbs_abort(
      "nbs_argument_error",
      "Yearbook HTML must be one raw vector or character value."
    )
  }
  decode <- function(from) {
    suppressWarnings(iconv(raw_html, from = from, to = "UTF-8", sub = NA))
  }
  if (!is.null(encoding)) {
    encoding <- .nbs_scalar_character(encoding, "encoding")
    decoded <- decode(encoding)
    if (is.na(decoded)) {
      .nbs_abort(
        "nbs_yearbook_schema_error",
        sprintf("Yearbook snapshot is not valid %s text.", encoding)
      )
    }
    return(decoded)
  }
  decoded <- decode("UTF-8")
  if (!is.na(decoded)) return(decoded)
  decoded <- decode("GB18030")
  if (is.na(decoded)) {
    .nbs_abort(
      "nbs_yearbook_schema_error",
      "Yearbook snapshot is neither valid UTF-8 nor GB18030 text."
    )
  }
  decoded
}

.nbs_yearbook_table_id <- function(x) {
  hit <- regmatches(x, regexec("^\\s*([0-9]+-[0-9]+)\\b", x, perl = TRUE))[[1L]]
  if (length(hit) == 2L) hit[[2L]] else NA_character_
}

#' Discover retained official NBS Yearbook table routes
#'
#' This function parses an already acquired Yearbook contents page. It never
#' makes a network request, allowing applications to review and freeze the
#' discovered routes before they are used for data ingestion.
#'
#' @param index_html HTML text from the official `left.htm` Yearbook index.
#' @param yearbook Four digit publication year.
#' @param index_url Official URL of `index_html`.
#' @return A data frame of discovered table IDs, titles and official URLs.
#' @export
nbs_yearbook_discover <- function(
    index_html,
    yearbook,
    index_url = .nbs_yearbook_index_url(yearbook)) {
  if (!is.character(index_html) || length(index_html) != 1L || is.na(index_html)) {
    .nbs_abort("nbs_argument_error", "`index_html` must be one HTML character value.")
  }
  yearbook <- .nbs_scalar_number(yearbook, "yearbook", 1949, 2100, integer = TRUE)
  index_url <- .nbs_scalar_character(index_url, "index_url")
  document <- tryCatch(xml2::read_html(index_html), error = function(error) error)
  if (inherits(document, "error")) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook index is not parseable HTML.",
      source_url = index_url, parent = document)
  }
  links <- rvest::html_elements(document, "a[href]")
  if (!length(links)) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook index has no table links.",
      source_url = index_url)
  }
  labels <- .nbs_yearbook_clean_text(rvest::html_text2(links))
  ids <- vapply(labels, .nbs_yearbook_table_id, character(1L))
  keep <- !is.na(ids)
  if (!any(keep)) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook index has no numbered table links.",
      source_url = index_url)
  }
  hrefs <- xml2::xml_attr(links, "href")
  routes <- data.frame(
    source_yearbook = rep(yearbook, sum(keep)),
    source_table = ids[keep],
    source_title = sub("^\\s*[0-9]+-[0-9]+\\s*", "", labels[keep]),
    source_url = vapply(hrefs[keep], xml2::url_absolute, character(1L),
      base = index_url),
    stringsAsFactors = FALSE
  )
  if (any(!grepl("/html/[^/]+\\.html?$", routes$source_url, ignore.case = TRUE))) {
    .nbs_abort("nbs_yearbook_schema_error", "A numbered Yearbook table route is not an HTML table URL.",
      source_url = index_url)
  }
  duplicate <- duplicated(routes$source_table) | duplicated(routes$source_table, fromLast = TRUE)
  if (any(duplicate)) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook index contains duplicate table IDs.",
      source_url = index_url)
  }
  routes <- routes[!duplicated(routes$source_table), , drop = FALSE]
  rownames(routes) <- NULL
  routes
}

.nbs_yearbook_targets_cache <- new.env(parent = emptyenv())

.nbs_load_yearbook_targets <- function() {
  if (exists("targets", envir = .nbs_yearbook_targets_cache, inherits = FALSE)) {
    return(get("targets", envir = .nbs_yearbook_targets_cache, inherits = FALSE))
  }
  targets <- utils::read.csv(
    .nbs_extdata_path("nbs-yearbook-targets.csv"), stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8"
  )
  required <- c("source_yearbook", "family", "source_table", "title_pattern",
    "schema_era", "expected_columns", "expected_min_rows", "header_rows")
  if (any(!required %in% names(targets)) || !nrow(targets) ||
      anyDuplicated(paste(targets$source_yearbook, targets$source_table))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook target manifest is invalid.")
  }
  targets$source_yearbook <- suppressWarnings(as.integer(targets$source_yearbook))
  targets$expected_columns <- suppressWarnings(as.integer(targets$expected_columns))
  targets$expected_min_rows <- suppressWarnings(as.integer(targets$expected_min_rows))
  targets$header_rows <- suppressWarnings(as.integer(targets$header_rows))
  if (anyNA(targets$source_yearbook) || anyNA(targets$expected_columns) ||
      anyNA(targets$expected_min_rows) || anyNA(targets$header_rows) ||
      any(targets$expected_columns < 2L) || any(targets$expected_min_rows < 1L) ||
      any(targets$header_rows < 1L) || any(!nzchar(targets$family)) ||
      any(!nzchar(targets$schema_era))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook target manifest has invalid fields.")
  }
  assign("targets", targets, envir = .nbs_yearbook_targets_cache)
  targets
}

#' Return frozen Yearbook target definitions
#'
#' Targets identify semantic tables by table number and title pattern; they do
#' not contain hard-coded source filenames or URLs.
#'
#' @param yearbook Optional publication year.
#' @return A data frame.
#' @export
nbs_yearbook_targets <- function(yearbook = NULL) {
  targets <- .nbs_load_yearbook_targets()
  if (!is.null(yearbook)) {
    yearbook <- .nbs_scalar_number(yearbook, "yearbook", 1949, 2100, integer = TRUE)
    targets <- targets[targets$source_yearbook == yearbook, , drop = FALSE]
  }
  rownames(targets) <- NULL
  targets
}

#' Resolve frozen Yearbook targets from a discovered official contents page
#'
#' @param index_html Official Yearbook contents-page HTML.
#' @param yearbook Four digit publication year.
#' @param index_url Official Yearbook contents URL.
#' @return Resolved acquisition routes with expected geometry and schema era.
#' @export
nbs_yearbook_routes <- function(
    index_html,
    yearbook,
    index_url = .nbs_yearbook_index_url(yearbook)) {
  routes <- nbs_yearbook_discover(index_html, yearbook, index_url)
  targets <- nbs_yearbook_targets(yearbook)
  if (!nrow(targets)) {
    .nbs_abort("nbs_yearbook_schema_error", sprintf("No frozen Yearbook targets exist for %d.", yearbook))
  }
  matched <- merge(targets, routes, by = c("source_yearbook", "source_table"),
    all.x = TRUE, sort = FALSE)
  if (any(is.na(matched$source_url))) {
    .nbs_abort("nbs_yearbook_schema_error", "A frozen Yearbook target was not discovered in the official contents page.",
      missing_tables = matched$source_table[is.na(matched$source_url)])
  }
  title_ok <- mapply(grepl, matched$title_pattern, matched$source_title,
    MoreArgs = list(ignore.case = TRUE, perl = TRUE), USE.NAMES = FALSE)
  if (any(!title_ok)) {
    .nbs_abort("nbs_yearbook_schema_error", "A discovered Yearbook title does not match its frozen target.",
      source_table = matched$source_table[!title_ok], source_title = matched$source_title[!title_ok])
  }
  matched <- matched[, c("source_yearbook", "family", "source_table", "source_title",
    "source_url", "schema_era", "expected_columns", "expected_min_rows", "header_rows"), drop = FALSE]
  rownames(matched) <- NULL
  matched
}

.nbs_yearbook_span <- function(node, name) {
  value <- suppressWarnings(as.integer(xml2::xml_attr(node, name)))
  if (is.na(value)) 1L else value
}

.nbs_yearbook_table_grid <- function(table_html, source_url = "") {
  document <- tryCatch(xml2::read_html(table_html), error = function(error) error)
  if (inherits(document, "error")) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table is not parseable HTML.", source_url = source_url, parent = document)
  }
  tables <- rvest::html_elements(document, "table")
  if (length(tables) != 1L) {
    .nbs_abort("nbs_yearbook_schema_error", "A Yearbook table document must contain exactly one HTML table.", source_url = source_url, table_count = length(tables))
  }
  rows <- rvest::html_elements(tables[[1L]], "tr")
  if (!length(rows)) .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table contains no rows.", source_url = source_url)
  values <- list()
  types <- list()
  numeric_hints <- list()
  for (row_index in seq_along(rows)) {
    cells <- xml2::xml_find_all(rows[[row_index]], "./th | ./td")
    if (!length(cells)) next
    column_index <- 1L
    for (cell in cells) {
      row_key <- as.character(row_index)
      while (!is.null(values[[row_key]]) && length(values[[row_key]]) >= column_index && !is.na(values[[row_key]][[column_index]])) column_index <- column_index + 1L
      rowspan <- .nbs_yearbook_span(cell, "rowspan")
      colspan <- .nbs_yearbook_span(cell, "colspan")
      if (rowspan < 1L || colspan < 1L) .nbs_abort("nbs_yearbook_schema_error", "A Yearbook table has an invalid row or column span.", source_url = source_url)
      text <- .nbs_yearbook_clean_text(rvest::html_text2(cell))
      type <- xml2::xml_name(cell)
      for (row_offset in seq_len(rowspan) - 1L) {
        target_row <- as.character(row_index + row_offset)
        if (is.null(values[[target_row]])) values[[target_row]] <- character()
        if (is.null(types[[target_row]])) types[[target_row]] <- character()
        if (is.null(numeric_hints[[target_row]])) numeric_hints[[target_row]] <- logical()
        for (column_offset in seq_len(colspan) - 1L) {
          target_column <- column_index + column_offset
          if (length(values[[target_row]]) >= target_column && !is.na(values[[target_row]][[target_column]])) .nbs_abort("nbs_yearbook_schema_error", "A Yearbook table has overlapping spans.", source_url = source_url)
          length(values[[target_row]]) <- max(length(values[[target_row]]), target_column)
          length(types[[target_row]]) <- max(length(types[[target_row]]), target_column)
          length(numeric_hints[[target_row]]) <- max(length(numeric_hints[[target_row]]), target_column)
          values[[target_row]][[target_column]] <- text
          types[[target_row]][[target_column]] <- type
          numeric_hints[[target_row]][[target_column]] <- !is.na(xml2::xml_attr(cell, "x:num"))
        }
      }
      column_index <- column_index + colspan
    }
  }
  counts <- vapply(values, function(x) sum(nzchar(x)), integer(1L))
  canonical_width <- as.integer(names(sort(table(counts[counts > 0L]), decreasing = TRUE))[[1L]])
  if (is.na(canonical_width) || canonical_width < 2L) .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table has no rectangular data geometry.", source_url = source_url)
  max_rows <- max(as.integer(names(values)))
  grid <- matrix("", nrow = max_rows, ncol = canonical_width)
  hints <- matrix(FALSE, nrow = max_rows, ncol = canonical_width)
  for (row_name in names(values)) {
    width <- min(length(values[[row_name]]), canonical_width)
    if (width) {
      row_index <- as.integer(row_name)
      grid[row_index, seq_len(width)] <- values[[row_name]][seq_len(width)]
      hints[row_index, seq_len(width)] <- numeric_hints[[row_name]][seq_len(width)]
    }
  }
  list(values = grid, numeric_hints = hints)
}

.nbs_yearbook_is_number <- function(x) grepl("^[+-]?(?:[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?|\\.[0-9]+)$", x)

.nbs_yearbook_value <- function(x, context) {
  x <- .nbs_yearbook_clean_text(x)
  if (!nzchar(x) || x %in% c("-", "--", "\u2014", "\u2026", "NA")) return(NULL)
  if (!.nbs_yearbook_is_number(x)) .nbs_abort("nbs_yearbook_schema_error", sprintf("%s has non-numeric data `%s`.", context, x))
  list(raw = x, value = as.numeric(gsub(",", "", x, fixed = TRUE)))
}

.nbs_yearbook_header_labels <- function(grid, header_end) {
  vapply(seq_len(ncol(grid)), function(column) {
    labels <- unique(grid[seq_len(header_end), column])
    labels <- labels[nzchar(labels)]
    labels <- labels[!grepl("^[0-9]+-[0-9]+\\b", labels)]
    paste(labels, collapse = " / ")
  }, character(1L))
}

.nbs_yearbook_reference_year <- function(row_label, column_header, fallback) {
  extract <- function(x) regmatches(x, regexpr("(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])", x, perl = TRUE))
  candidate <- extract(row_label)
  if (length(candidate) && !is.na(candidate) && nzchar(candidate)) return(as.integer(candidate))
  candidate <- extract(column_header)
  if (length(candidate) && !is.na(candidate) && nzchar(candidate)) return(as.integer(candidate))
  as.integer(fallback)
}

#' Parse a frozen official NBS Yearbook table to normalized long rows
#'
#' The parser expands HTML `rowspan`/`colspan` cells, rejects changed geometry,
#' and records every source/provenance field required to reproduce a result.
#'
#' @param table_html One locally stored Yearbook table HTML document.
#' @param route One row returned by [nbs_yearbook_routes()].
#' @param retrieved_at Retrieval time of the frozen document.
#' @param raw_sha256 Optional SHA-256 of the raw document; computed when absent.
#' @return A long data frame with a JSON `dimensions` column.
#' @export
nbs_yearbook_parse_table <- function(table_html, route, retrieved_at = Sys.time(), raw_sha256 = NULL) {
  if (!is.character(table_html) || length(table_html) != 1L || is.na(table_html)) .nbs_abort("nbs_argument_error", "`table_html` must be one HTML character value.")
  needed <- c("source_yearbook", "family", "source_table", "source_url", "schema_era", "expected_columns", "expected_min_rows", "header_rows")
  if (!is.data.frame(route) || nrow(route) != 1L || any(!needed %in% names(route))) .nbs_abort("nbs_argument_error", "`route` must be one resolved Yearbook route row.")
  if (!inherits(retrieved_at, "POSIXt") || length(retrieved_at) != 1L || is.na(retrieved_at)) .nbs_abort("nbs_argument_error", "`retrieved_at` must be one non-missing POSIXct value.")
  raw_sha256 <- raw_sha256 %||% .nbs_sha256(charToRaw(enc2utf8(table_html)))
  raw_sha256 <- .nbs_scalar_character(raw_sha256, "raw_sha256")
  if (!grepl("^[0-9a-f]{64}$", raw_sha256)) .nbs_abort("nbs_argument_error", "`raw_sha256` must be a lowercase SHA-256 hex digest.")
  parsed_grid <- .nbs_yearbook_table_grid(table_html, route$source_url[[1L]])
  grid <- parsed_grid$values
  nonempty <- rowSums(matrix(nzchar(grid), nrow = nrow(grid), ncol = ncol(grid)))
  hinted_rows <- which(rowSums(parsed_grid$numeric_hints[, -1L, drop = FALSE]) >= 1L & nzchar(grid[, 1L]))
  data_candidate <- if (length(hinted_rows)) hinted_rows else which(vapply(seq_len(nrow(grid)), function(row) sum(.nbs_yearbook_is_number(grid[row, -1L])) >= 1L && nzchar(grid[row, 1L]), logical(1L)))
  if (!length(data_candidate)) .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table has no numeric data rows.", source_url = route$source_url[[1L]])
  header_end <- as.integer(route$header_rows[[1L]])
  data_start <- header_end + 1L
  if (header_end < 1L || data_start > nrow(grid) ||
      !data_start %in% data_candidate ||
      ncol(grid) != as.integer(route$expected_columns[[1L]]) ||
      sum(nonempty >= 1L) < as.integer(route$expected_min_rows[[1L]])) {
    .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table geometry differs from its frozen contract.", source_url = route$source_url[[1L]], actual_columns = ncol(grid), actual_rows = sum(nonempty >= 1L), expected_columns = as.integer(route$expected_columns[[1L]]), expected_min_rows = as.integer(route$expected_min_rows[[1L]]))
  }
  headers <- .nbs_yearbook_header_labels(grid, header_end)
  records <- list(); section <- ""
  for (row in seq.int(data_start, nrow(grid))) {
    label <- .nbs_yearbook_clean_text(grid[row, 1L])
    if (!nzchar(label)) next
    values <- grid[row, -1L]
    if (!sum(.nbs_yearbook_is_number(values))) {
      if (all(!nzchar(values))) section <- label
      next
    }
    for (column in seq.int(2L, ncol(grid))) {
      cell <- .nbs_yearbook_value(grid[row, column], sprintf("Yearbook %s row %d column %d", route$source_table[[1L]], row, column))
      if (is.null(cell)) next
      column_header <- headers[[column]]
      reference_year <- .nbs_yearbook_reference_year(label, column_header, route$source_yearbook[[1L]] - 1L)
      measure <- if (grepl("^(?:19|20)[0-9]{2}$", column_header)) "value" else column_header
      if (!nzchar(measure)) .nbs_abort("nbs_yearbook_schema_error", "A Yearbook value column has no semantic header.", source_url = route$source_url[[1L]])
      dimensions <- list(item = label)
      if (nzchar(section)) dimensions$section <- section
      records[[length(records) + 1L]] <- data.frame(source_yearbook = as.integer(route$source_yearbook[[1L]]), source_table = route$source_table[[1L]], reference_year = as.integer(reference_year), family = route$family[[1L]], measure = measure, dimensions = jsonlite::toJSON(dimensions, auto_unbox = TRUE, null = "null"), value = cell$value, raw_value = cell$raw, source_url = route$source_url[[1L]], retrieved_at = as.POSIXct(retrieved_at, tz = "UTC"), raw_sha256 = raw_sha256, schema_era = route$schema_era[[1L]], stringsAsFactors = FALSE)
    }
  }
  if (!length(records)) .nbs_abort("nbs_yearbook_schema_error", "The Yearbook table produced no numeric observations.", source_url = route$source_url[[1L]])
  result <- do.call(rbind, records); rownames(result) <- NULL; result
}

#' Explicitly retrieve a Yearbook contents page for maintenance discovery
#'
#' This is the only live operation in the Yearbook foundation. Normal table
#' parsing never uses the network.
#'
#' @param yearbook Four digit publication year.
#' @param client An object returned by [nbs_client()].
#' @return A discovered table data frame with retrieval provenance attributes.
#' @export
nbs_yearbook_discover_live <- function(yearbook, client = nbs_client()) {
  yearbook <- .nbs_scalar_number(yearbook, "yearbook", 1949, 2100, integer = TRUE)
  response <- .nbs_request_document(client, .nbs_yearbook_index_url(yearbook), operation = "NBS Yearbook contents request")
  result <- nbs_yearbook_discover(response$body_text, yearbook, response$url)
  attr(result, "retrieved_at") <- response$retrieved_at
  attr(result, "raw_sha256") <- response$raw_sha256
  result
}

.nbs_yearbook_snapshot_cache <- new.env(parent = emptyenv())

.nbs_load_yearbook_snapshot_index <- function() {
  if (exists("snapshot_index", envir = .nbs_yearbook_snapshot_cache,
      inherits = FALSE)) {
    return(get("snapshot_index", envir = .nbs_yearbook_snapshot_cache,
      inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-yearbook-snapshot-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8"
  )
  required <- c(
    "source_yearbook", "source_table", "family", "dataset_id",
    "source_url", "asset_path", "encoding", "retrieved_at", "raw_sha256",
    "schema_era"
  )
  if (!nrow(index) || any(!required %in% names(index)) ||
      anyDuplicated(paste(index$source_yearbook, index$source_table, sep = "\r")) ||
      any(!index$encoding %in% c("UTF-8", "GB18030")) ||
      any(!grepl("^[0-9a-f]{64}$", index$raw_sha256)) ||
      any(!nzchar(index$dataset_id)) || any(!nzchar(index$schema_era))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook snapshot index is invalid.")
  }
  index$source_yearbook <- suppressWarnings(as.integer(index$source_yearbook))
  if (anyNA(index$source_yearbook) || anyNA(as.POSIXct(index$retrieved_at, tz = "UTC"))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook snapshot index has invalid dates.")
  }
  assign("snapshot_index", index, envir = .nbs_yearbook_snapshot_cache)
  index
}

.nbs_yearbook_snapshot_raw <- function(snapshot) {
  path <- .nbs_extdata_path(snapshot$asset_path[[1L]])
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  actual <- .nbs_sha256(raw)
  if (!identical(actual, snapshot$raw_sha256[[1L]])) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("Frozen Yearbook snapshot hash differs for table %s.",
        snapshot$source_table[[1L]])
    )
  }
  raw
}

.nbs_yearbook_default_mapping <- function(rows) {
  required <- c(
    "source_yearbook", "source_table", "reference_year", "family",
    "dimensions", "measure", "schema_era", "source_row_index"
  )
  if (any(!required %in% names(rows))) {
    .nbs_abort("nbs_yearbook_schema_error", "Yearbook rows have no mapping fields.")
  }
  item <- vapply(rows$dimensions, function(value) {
    parsed <- jsonlite::fromJSON(value)
    paste(
      .nbs_yearbook_clean_text(parsed$section %||% ""),
      .nbs_yearbook_clean_text(parsed$item %||% ""),
      sep = " / "
    ) |> .nbs_yearbook_clean_text()
  }, character(1L))
  source_measure <- .nbs_yearbook_clean_text(rows$measure)
  source_label <- item
  measure_key <- gsub("\\s+", "", source_measure, perl = TRUE)
  demographic <- c(
    "\u603B\u4EBA\u53E3(\u5E74\u672B)" = "population_year_end",
    "\u6309\u6027\u522B\u5206/\u7537/\u4EBA\u53E3\u6570" = "male_population",
    "\u6309\u6027\u522B\u5206/\u7537/\u6BD4\u91CD(%)" = "male_share_percent",
    "\u6309\u6027\u522B\u5206/\u5973/\u4EBA\u53E3\u6570" = "female_population",
    "\u6309\u6027\u522B\u5206/\u5973/\u6BD4\u91CD(%)" = "female_share_percent",
    "\u6309\u57CE\u4E61\u5206/\u57CE\u9547/\u4EBA\u53E3\u6570" = "urban_population",
    "\u6309\u57CE\u4E61\u5206/\u57CE\u9547/\u6BD4\u91CD(%)" = "urban_share_percent",
    "\u6309\u57CE\u4E61\u5206/\u4E61\u6751/\u4EBA\u53E3\u6570" = "rural_population",
    "\u6309\u57CE\u4E61\u5206/\u4E61\u6751/\u6BD4\u91CD(%)" = "rural_share_percent"
  )
  wage <- c(
    "\u5408\u8BA1" = "average_wage_total",
    "\u56FD\u6709/\u5355\u4F4D" = "average_wage_state_owned_units",
    "\u57CE\u9547\u96C6\u4F53/\u5355\u4F4D" = "average_wage_urban_collective_units",
    "\u5176\u4ED6/\u5355\u4F4D" = "average_wage_other_urban_units"
  )
  industrial <- c(
    "\u4F01\u4E1A/\u5355\u4F4D\u6570/(\u4E2A)" = "enterprise_count",
    "\u5DE5\u4E1A/\u603B\u4EA7\u503C/(\u5F53\u5E74\u4EF7\u683C)" = "industrial_gross_output_current_prices",
    "\u8D44\u4EA7\u603B\u8BA1" = "total_assets",
    "\u4E3B\u8425\u4E1A\u52A1/\u6536\u5165" = "main_business_revenue",
    "\u5229\u6DA6\u603B\u989D" = "total_profit",
    "\u5168\u90E8\u4ECE\u4E1A/\u4EBA\u5458\u5E74\u5E73\u5747/\u4EBA\u6570(\u4E07\u4EBA)" = "annual_average_employees"
  )
  canonical_measure <- rep("published_level", nrow(rows))
  demographic_rows <- rows$family == "demography"
  wage_rows <- rows$family == "wages"
  industrial_rows <- rows$family == "industrial_benefits"
  retail_rows <- rows$family == "retail"
  canonical_measure[demographic_rows] <- demographic[measure_key[demographic_rows]]
  canonical_measure[wage_rows] <- wage[measure_key[wage_rows]]
  canonical_measure[industrial_rows] <- industrial[measure_key[industrial_rows]]
  canonical_measure[retail_rows] <- ifelse(
    grepl("\u793E\u4F1A\u6D88\u8D39\u54C1\u96F6\u552E\u603B\u989D", source_measure[retail_rows], fixed = TRUE),
    "retail_sales_total", "retail_sales_growth_percent"
  )
  if (anyNA(canonical_measure)) {
    .nbs_abort(
      "nbs_yearbook_schema_error",
      "A Yearbook table has an unmapped official measure label.",
      source_table = unique(rows$source_table[is.na(canonical_measure)])
    )
  }
  key <- paste(rows$source_yearbook, rows$source_table, rows$reference_year,
    rows$source_row_index, source_label, source_measure, sep = "\r")
  code <- vapply(key, function(value) {
    paste0("yb_", substr(digest::digest(value, algo = "sha256"), 1L, 16L))
  }, character(1L))
  unit <- ifelse(
    grepl("%", source_measure, fixed = TRUE) | grepl("%", source_label, fixed = TRUE),
    "Percent", "Official published unit"
  )
  data.frame(
    source_yearbook = rows$source_yearbook,
    source_table = rows$source_table,
    reference_year = rows$reference_year,
    source_row_index = rows$source_row_index,
    source_label_zh = source_label,
    source_measure_zh = source_measure,
    canonical_series = code,
    canonical_measure = canonical_measure,
    dimension_label_en = paste0("Official Yearbook ", rows$source_table,
      " series ", code),
    dimension_label_zh = paste(source_label, source_measure, sep = " / "),
    unit_en = unit,
    unit_zh = ifelse(unit == "Percent", "\u767E\u5206\u6BD4", "\u5B98\u65B9\u53D1\u5E03\u5355\u4F4D"),
    schema_era = rows$schema_era,
    stringsAsFactors = FALSE
  )
}

.nbs_load_yearbook_label_map <- function() {
  path <- .nbs_extdata_path("nbs-yearbook-label-map.csv")
  map <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character(), fileEncoding = "UTF-8")
  required <- c("source_yearbook", "source_table", "reference_year", "source_row_index",
    "source_label_zh", "source_measure_zh", "canonical_series",
    "canonical_measure", "dimension_label_en", "dimension_label_zh",
    "unit_en", "unit_zh", "schema_era")
  key <- paste(map$source_yearbook, map$source_table, map$reference_year,
    map$source_row_index,
    map$source_label_zh, map$source_measure_zh, sep = "\r")
  if (!nrow(map) || any(!required %in% names(map)) || anyDuplicated(key) ||
      any(!nzchar(map$canonical_series)) || any(!nzchar(map$canonical_measure))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook Chinese-label map is invalid.")
  }
  map
}

.nbs_yearbook_adapt_rows <- function(rows, dataset_id, label_map = .nbs_load_yearbook_label_map()) {
  rows <- rows[rows$reference_year == 2010L, , drop = FALSE]
  if (!"source_row_index" %in% names(rows)) {
    .nbs_abort("nbs_yearbook_schema_error", "Yearbook rows have no source row index.")
  }
  source_label <- vapply(rows$dimensions, function(value) {
    parsed <- jsonlite::fromJSON(value)
    paste(
      .nbs_yearbook_clean_text(parsed$section %||% ""),
      .nbs_yearbook_clean_text(parsed$item %||% ""),
      sep = " / "
    ) |> .nbs_yearbook_clean_text()
  }, character(1L))
  source_measure <- .nbs_yearbook_clean_text(rows$measure)
  key <- paste(rows$source_yearbook, rows$source_table, rows$reference_year,
    rows$source_row_index,
    source_label, source_measure, sep = "\r")
  map_key <- paste(label_map$source_yearbook, label_map$source_table,
    label_map$reference_year, label_map$source_row_index, label_map$source_label_zh,
    label_map$source_measure_zh, sep = "\r")
  matched <- match(key, map_key)
  if (anyNA(matched)) {
    .nbs_abort(
      "nbs_yearbook_schema_error",
      "Frozen Yearbook rows have no approved Chinese-label mapping.",
      source_table = unique(rows$source_table[is.na(matched)])
    )
  }
  map <- label_map[matched, , drop = FALSE]
  data.frame(
    dataset_id = dataset_id,
    release_period = as.character(rows$reference_year),
    series = map$canonical_series,
    measure = map$canonical_measure,
    period_basis = "annual",
    raw_value = rows$raw_value,
    value = rows$value,
    # Annual observations use the canonical period key shared by the scalar
    # and dimensional APIs.  The reference window still records the complete
    # calendar year in dataset result metadata.
    observation_date = sprintf("%d-01-01", rows$reference_year),
    source_yearbook = rows$source_yearbook,
    source_table = rows$source_table,
    source_label_zh = map$source_label_zh,
    source_measure_zh = map$source_measure_zh,
    dimension_label_en = map$dimension_label_en,
    dimension_label_zh = map$dimension_label_zh,
    unit_en = map$unit_en,
    unit_zh = map$unit_zh,
    schema_era = map$schema_era,
    row_index = as.integer(rows$source_row_index),
    column_index = 1L,
    stringsAsFactors = FALSE
  )
}

.nbs_yearbook_archive_cache <- new.env(parent = emptyenv())

.nbs_load_yearbook_release_index <- function() {
  if (exists("release_index", envir = .nbs_yearbook_archive_cache,
      inherits = FALSE)) {
    return(get("release_index", envir = .nbs_yearbook_archive_cache,
      inherits = FALSE))
  }
  index <- utils::read.csv(.nbs_extdata_path("nbs-yearbook-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
    na.strings = character(), fileEncoding = "UTF-8")
  required <- c("manifest_version", "dataset_id", "period",
    "reference_period_start", "reference_period_end", "release_date",
    "source_url", "schema_era", "expected_yearbook_cells", "storage",
    "snapshot_sha256", "source_capture_sha256")
  if (!nrow(index) || any(!required %in% names(index)) ||
      anyDuplicated(paste(index$dataset_id, index$period, sep = "\r")) ||
      any(index$storage != "yearbook_snapshot") ||
      any(!grepl("^[0-9a-f]{64}$", index$snapshot_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook release index is invalid.")
  }
  index$expected_yearbook_cells <- suppressWarnings(as.integer(index$expected_yearbook_cells))
  if (anyNA(index$expected_yearbook_cells) || any(index$expected_yearbook_cells < 1L)) {
    .nbs_abort("nbs_manifest_error", "The Yearbook release index has invalid cardinalities.")
  }
  assign("release_index", index, envir = .nbs_yearbook_archive_cache)
  index
}

.nbs_load_yearbook_observations <- function() {
  if (exists("observations", envir = .nbs_yearbook_archive_cache,
      inherits = FALSE)) {
    return(get("observations", envir = .nbs_yearbook_archive_cache,
      inherits = FALSE))
  }
  path <- .nbs_extdata_path("nbs-yearbook-observations.csv.gz")
  rows <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character(), fileEncoding = "UTF-8")
  required <- c("dataset_id", "release_period", "series", "measure",
    "period_basis", "raw_value", "value", "observation_date",
    "source_yearbook", "source_table", "source_label_zh", "schema_era")
  actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  expected <- unique(.nbs_load_yearbook_release_index()$snapshot_sha256)
  if (!nrow(rows) || any(!required %in% names(rows)) ||
      length(expected) != 1L || !identical(actual, expected) ||
      any(!is.finite(suppressWarnings(as.numeric(rows$value))))) {
    .nbs_abort("nbs_manifest_error", "The Yearbook observation archive is invalid.")
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(rows$observation_date)
  assign("observations", rows, envir = .nbs_yearbook_archive_cache)
  rows
}
