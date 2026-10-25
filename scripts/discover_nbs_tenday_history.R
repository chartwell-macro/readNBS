#!/usr/bin/env Rscript

# Discover and freeze the official 2014--19 ten-day production-market releases.
# The NBS Chinese search API is used only at maintenance time.  It returns
# migrated official pages whose URL date is often 2023, so period membership is
# determined from the release title, never from the URL or search document date.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: discover_nbs_tenday_history.R OUTPUT_TARGETS_CSV CAPTURES_DIR",
    call. = FALSE
  )
}
output_path <- normalizePath(args[[1L]], mustWork = FALSE)
captures_dir <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(captures_dir, recursive = TRUE, showWarnings = FALSE)
years_text <- Sys.getenv("READNBS_TENDAY_DISCOVERY_YEARS", "")
years <- if (nzchar(years_text)) {
  as.integer(strsplit(years_text, ",", fixed = TRUE)[[1L]])
} else {
  2014:2019
}
if (!length(years) || anyNA(years) || any(years < 2014L | years > 2019L)) {
  stop("READNBS_TENDAY_DISCOVERY_YEARS must be a comma-separated subset of 2014:2019.", call. = FALSE)
}

query <- "流通领域重要生产资料市场价格变动情况"

search_page <- function(year, page) {
  response <- httr2::request("https://api.so-gov.cn/query/s") |>
    # The public IPv4 endpoint may rate-limit maintenance discovery while the
    # same official service remains available over IPv6.
    httr2::req_options(ipresolve = 2L) |>
    httr2::req_body_form(
      siteCode = "bm36000002", tab = "all", page = page, pageSize = 20L,
      qt = query, timeOption = 2L,
      startDateStr = sprintf("%d-01-01", year),
      # The last dekad is normally released in the following January; Lunar
      # New Year can delay February's final release into March.
      endDateStr = sprintf("%d-03-31", year + 1L),
      sort = "dateDesc", keyPlace = 1L, fileType = ""
    ) |>
    httr2::req_retry(max_tries = 4L) |>
    httr2::req_perform()
  httr2::resp_body_json(response, simplifyVector = FALSE)
}

all_documents <- function(year) {
  first <- search_page(year, 1L)
  pages <- max(1L, ceiling((first$totalHits %||% 0L) / 20L))
  results <- c(list(first), if (pages > 1L) lapply(seq.int(2L, pages), function(i) search_page(year, i)) else list())
  docs <- unlist(lapply(results, function(x) x$resultDocs %||% list()), recursive = FALSE)
  if (!length(docs)) return(data.frame(title = character(), href = character(), stringsAsFactors = FALSE))
  data.frame(
    title = vapply(docs, function(x) x$data$titleO %||% "", character(1L)),
    href = vapply(docs, function(x) x$data$url %||% "", character(1L)),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

period_from_title <- function(title) {
  legacy <- regmatches(title, regexec(
    "^流通领域重要生产资料市场价格变动情况（(201[4-9])年([0-9]{1,2})月([0-9]{1,2})日?[-—](?:[0-9]{1,2})日?）$",
    title, perl = TRUE
  ))[[1L]]
  if (length(legacy) == 4L) {
    year <- as.integer(legacy[[2L]])
    month <- as.integer(legacy[[3L]])
    day <- as.integer(legacy[[4L]])
  } else {
    modern <- regmatches(title, regexec(
      "^(201[4-9])年([0-9]{1,2})月([上中下])旬流通领域重要生产资料市场价格变动情况$",
      title, perl = TRUE
    ))[[1L]]
    if (length(modern) != 4L) return(NULL)
    year <- as.integer(modern[[2L]])
    month <- as.integer(modern[[3L]])
    day <- c("上" = 1L, "中" = 11L, "下" = 21L)[[modern[[4L]]]]
  }
  end <- if (day == 1L) 10L else if (day == 11L) 20L else as.integer(format(
    seq(as.Date(sprintf("%04d-%02d-01", year, month)), by = "month", length.out = 2L)[[2L]] - 1L,
    "%d"
  ))
  data.frame(
    period = sprintf("%04d-%02d-D%d", year, month, if (day == 1L) 1L else if (day == 11L) 2L else 3L),
    reference_period_start = sprintf("%04d-%02d-%02d", year, month, day),
    reference_period_end = sprintf("%04d-%02d-%02d", year, month, end),
    stringsAsFactors = FALSE
  )
}

found <- do.call(rbind, lapply(years, all_documents))
found$title <- trimws(found$title)
found <- found[grepl("^https://www\\.stats\\.gov\\.cn/", found$href), , drop = FALSE]
periods <- lapply(found$title, period_from_title)
keep <- !vapply(periods, is.null, logical(1L))
found <- found[keep, , drop = FALSE]
targets <- data.frame(
  period = unname(vapply(periods[keep], `[[`, character(1L), "period")),
  reference_period_start = unname(vapply(periods[keep], `[[`, character(1L), "reference_period_start")),
  reference_period_end = unname(vapply(periods[keep], `[[`, character(1L), "reference_period_end")),
  title = found$title,
  href = found$href,
  stringsAsFactors = FALSE
)
targets <- targets[substr(targets$period, 1L, 4L) %in% as.character(years), , drop = FALSE]
targets <- targets[order(targets$period, targets$href), , drop = FALSE]
targets <- targets[!duplicated(targets$period), , drop = FALSE]
expected <- unlist(lapply(years, function(year) unlist(lapply(1:12, function(month) {
  sprintf("%04d-%02d-D%d", year, month, 1:3)
}))), use.names = FALSE)
# No official publication exists for these Lunar New Year collection windows.
# They are absent from the official NBS search results when searched across the
# following publication quarter, so retain the documented calendar gap rather
# than synthesising an otherwise regular ten-day window.
known_unpublished_periods <- c(
  "2015-02-D2", "2016-02-D1", "2017-01-D3", "2018-02-D2",
  "2019-02-D1", "2019-05-D2"
)
expected <- setdiff(expected, known_unpublished_periods)
if (nrow(targets) != length(expected) || !setequal(targets$period, expected)) {
  missing <- setdiff(sort(expected), targets$period)
  stop(
    sprintf("Discovery did not yield every expected official release period (missing: %s).", paste(missing, collapse = ", ")),
    call. = FALSE
  )
}
targets$source_capture_file <- file.path(
  captures_dir, paste0(gsub("-", "", targets$period, fixed = TRUE), ".html")
)
if (Sys.getenv("READNBS_TENDAY_SKIP_DOWNLOAD", "") != "1") {
  is_official_capture <- function(path, expected_title) {
    if (!file.exists(path)) return(FALSE)
    raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    grepl(expected_title, raw, fixed = TRUE) && grepl("<table", raw, fixed = TRUE) &&
      !grepl("Please enable JavaScript", raw, fixed = TRUE)
  }
  download_one <- function(i) {
    path <- targets$source_capture_file[[i]]
    if (is_official_capture(path, targets$title[[i]])) return(invisible(path))
    for (attempt in seq_len(5L)) {
      response <- httr2::request(targets$href[[i]]) |>
        httr2::req_options(ipresolve = 2L) |>
        # Match the lightweight transport used by the official site’s public
        # pages; its browser-oriented WAF otherwise returns a JavaScript
        # challenge instead of the retained release HTML.
        httr2::req_user_agent(paste0(
          "curl/8.7.1 (readNBS archived ten-day release ", targets$period[[i]], ")"
        )) |>
        httr2::req_retry(max_tries = 4L) |>
        httr2::req_perform()
      raw <- httr2::resp_body_raw(response)
      text <- rawToChar(raw)
      if (grepl(targets$title[[i]], text, fixed = TRUE) &&
          grepl("<table", text, fixed = TRUE) &&
          !grepl("Please enable JavaScript", text, fixed = TRUE)) {
        writeBin(raw, path)
        return(invisible(path))
      }
      Sys.sleep(attempt)
    }
    stop("NBS did not return an official table capture for ", targets$period[[i]], call. = FALSE)
  }
  download_workers <- suppressWarnings(as.integer(
    Sys.getenv("READNBS_TENDAY_DOWNLOAD_CONCURRENCY", "1")
  ))
  if (is.na(download_workers) || download_workers < 1L) {
    stop("READNBS_TENDAY_DOWNLOAD_CONCURRENCY must be a positive integer.", call. = FALSE)
  }
  # Keep a deterministic sequential default.  Parallel retrieval only writes
  # disjoint, named official captures and makes a full historical refresh
  # practical when the maintainer explicitly opts in.
  if (download_workers == 1L || .Platform$OS.type == "windows") {
    invisible(lapply(seq_len(nrow(targets)), download_one))
  } else {
    invisible(parallel::mclapply(
      seq_len(nrow(targets)), download_one,
      mc.cores = min(download_workers, nrow(targets)), mc.preschedule = FALSE
    ))
  }
}
targets$source_capture_sha256 <- vapply(targets$source_capture_file, function(path) {
  if (!file.exists(path)) return("")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}, character(1L))
# The target manifest is committed with the archive, so do not leak the local
# worktree path.  The offline builder intentionally resolves this by basename
# against the supplied captures directory.
targets$source_capture_file <- file.path(
  basename(captures_dir), basename(targets$source_capture_file)
)
utils::write.csv(targets, output_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("Wrote %d exact official ten-day targets to %s\n", nrow(targets), output_path))
