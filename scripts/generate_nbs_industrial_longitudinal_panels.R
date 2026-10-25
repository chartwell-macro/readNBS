#!/usr/bin/env Rscript

# Derive conservative chart-ready industrial histories from the verified
# official UUID-era manifest. A concept is joined only when its full published
# English title, unit, period policy and aggregation basis match exactly in
# adjacent classification/registration eras and the observed monthly spans are
# continuous under the NBS January-omission calendar.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else file.path("inst", "extdata")
if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

version <- "2026-08-18.2"
source_manifest <- utils::read.csv(
  gzfile(file.path(output_dir, "nbs-industrial-series-manifest.csv.gz")),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  colClasses = "character",
  na.strings = character(),
  fileEncoding = "UTF-8"
)

era_order <- c(
  "gb-t-4754-2002-2003-2011" = 1L,
  "gb-t-4754-2011-2012-2017" = 2L,
  "gb-t-4754-2017-2018-present" = 3L,
  "registration-type-through-2023" = 1L,
  "registration-type-2024-present" = 2L
)

expected_dates <- function(start, end, period_policy) {
  dates <- seq(as.Date(start), as.Date(end), by = "month")
  if (period_policy %in% c(
      "january_not_reported_february_first_period",
      "ytd_january_not_reported_february_combined")) {
    dates <- dates[format(dates, "%m") != "01"]
  }
  dates
}

group_kind <- function(rows) {
  if (all(startsWith(rows$classification_id, "gb-t-"))) return("industry")
  if (all(startsWith(rows$classification_id, "registration-type-"))) {
    return("registration")
  }
  ""
}

is_safe_group <- function(rows) {
  kind <- group_kind(rows)
  positions <- sort(unique(unname(era_order[rows$classification_id])))
  if (!nzchar(kind) || length(positions) < 2L || anyNA(positions) ||
      any(diff(positions) != 1L)) {
    return(FALSE)
  }
  invariant <- c("unit_en", "unit_zh", "period_policy", "aggregation_basis")
  if (any(vapply(invariant, function(column) {
    length(unique(rows[[column]])) != 1L
  }, logical(1L)))) {
    return(FALSE)
  }
  dates <- expected_dates(
    min(as.Date(rows$slice_start)),
    max(as.Date(rows$slice_end)),
    rows$period_policy[[1L]]
  )
  all(vapply(dates, function(date) {
    any(date >= as.Date(rows$slice_start) & date <= as.Date(rows$slice_end))
  }, logical(1L)))
}

groups <- split(source_manifest, trimws(source_manifest$title_en))
groups <- groups[vapply(groups, is_safe_group, logical(1L))]
if (length(groups) != 2358L) {
  stop("The reviewed industrial longitudinal group count has changed.")
}

make_id <- function(title, rows) {
  kind <- toupper(group_kind(rows))
  additive_ytd <- identical(rows$aggregation_basis[[1L]], "year_to_date") &&
    identical(rows$unit_en[[1L]], "100 million yuan") &&
    grepl(", Accumulated \\(", title) &&
    !grepl("Same Period of Previous Year|Growth Rate", title)
  suffix <- if (additive_ytd) {
    "YTD_LEVEL"
  } else if (identical(rows$aggregation_basis[[1L]], "year_to_date")) {
    "YTD_PUBLISHED"
  } else {
    "END_PERIOD"
  }
  hash <- toupper(substr(digest::digest(
    paste(kind, title, sep = "\r"), algo = "sha256", serialize = FALSE
  ), 1L, 20L))
  paste(
    "NBS", "CN", "M", "INDUSTRIAL_ENTERPRISES", "LONGITUDINAL",
    kind, hash, suffix, sep = "/"
  )
}

long_rows <- list()
dimension_rows <- list()
for (title in names(groups)) {
  rows <- groups[[title]]
  rows <- rows[order(as.Date(rows$slice_start)), , drop = FALSE]
  kind <- group_kind(rows)
  stable_id <- make_id(title, rows)
  default_start <- min(as.Date(rows$slice_start))
  public_note_en <- paste(
    "Exact-title longitudinal history across adjacent official",
    if (kind == "industry") "GB/T industry-classification" else "registration-type",
    "eras. Values are not rebased or interpolated; each observation retains",
    "its official classification ID and UUID provenance."
  )
  public_note_zh <- "跨相邻官方分类时期、按完全相同发布标题连接的纵向历史；不重定基、不插值，并保留逐期分类及UUID来源。"

  rows$manifest_version <- version
  rows$series_id <- stable_id
  rows$title_en <- title
  rows$title_zh <- rows$title_zh[[1L]]
  rows$description_en <- paste("Official NBS longitudinal portal series:", title)
  rows$description_zh <- rows$title_zh[[1L]]
  rows$default_start <- as.character(default_start)
  rows$stitch_policy <- "exact_title_adjacent_official_eras"
  rows$series_note_en <- public_note_en
  rows$series_note_zh <- public_note_zh
  rows$source_url <- "https://data.stats.gov.cn/"
  rows$source_note <- paste(
    "Latest/revised NBS National Data portal; exact row-level catalogue and",
    "indicator UUIDs identify every classification-era slice."
  )
  long_rows[[length(long_rows) + 1L]] <- rows

  dimension_rows[[length(dimension_rows) + 1L]] <- data.frame(
    manifest_version = version,
    dataset_id = if (kind == "industry") {
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL"
    } else {
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL"
    },
    dimension = "series_id",
    value = stable_id,
    label_en = title,
    label_zh = rows$title_zh[[1L]],
    route_value = stable_id,
    sort_order = 0L,
    valid_from = as.character(default_start),
    valid_to = max(rows$slice_end),
    unit_en = rows$unit_en[[1L]],
    unit_zh = rows$unit_zh[[1L]],
    semantics = if (identical(rows$aggregation_basis[[1L]], "year_to_date")) {
      "year_to_date"
    } else {
      "end_period"
    },
    stringsAsFactors = FALSE
  )
}

long_manifest <- do.call(rbind, long_rows)
dimensions <- do.call(rbind, dimension_rows)
dimensions <- dimensions[order(dimensions$dataset_id, dimensions$label_en), ]
dimensions$sort_order <- ave(
  seq_len(nrow(dimensions)), dimensions$dataset_id, FUN = seq_along
)
rownames(dimensions) <- NULL

dataset_definition <- function(kind) {
  industry <- identical(kind, "industry")
  members <- dimensions$dataset_id == if (industry) {
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/LONGITUDINAL"
  } else {
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/LONGITUDINAL"
  }
  data.frame(
    manifest_version = version,
    dataset_id = unique(dimensions$dataset_id[members]),
    release_id = "cn_nbs_industrial_profits",
    family = "industrial_profits",
    title_en = if (industry) {
      "Industrial financial indicators by industry — longitudinal series"
    } else {
      "Industrial financial indicators by registration type — longitudinal series"
    },
    title_zh = if (industry) "分行业工业企业财务指标—纵向序列" else "分登记注册类型工业企业财务指标—纵向序列",
    description_en = paste(
      "Chart-ready latest-revised histories joined only where the complete",
      "official title and measurement semantics match across adjacent eras."
    ),
    description_zh = "仅在完整官方标题及计量语义跨相邻时期完全一致时连接的制图用最新修订历史。",
    frequency = "M",
    route = "portal_uuid_panel",
    default_start = min(dimensions$valid_from[members]),
    available_to = max(dimensions$valid_to[members]),
    coverage_en = paste(sum(members), "continuous exact-title official portal histories"),
    coverage_zh = paste(sum(members), "项按完全相同标题连接的官方门户连续历史"),
    required_dimensions = "series_id",
    panel_allowed = "TRUE",
    expected_rows = sum(members),
    expected_cells = sum(members),
    aggregation_basis = "mixed_span",
    seasonal_adjustment = "not_seasonally_adjusted",
    derived_method = "none",
    source_vintage = "latest_revised_database",
    series_note_en = paste(
      "No interpolation or rebasing. Classification IDs and exact UUIDs",
      "remain in row provenance; unmatched concepts stay in the source-era archives."
    ),
    series_note_zh = "不插值、不重定基；逐期保留分类ID及精确UUID，无法匹配的指标仍保留在分时期来源档案。",
    unit_en = "Official published unit",
    unit_zh = "官方发布单位",
    source_url = "https://data.stats.gov.cn/",
    source_note = "Conservative exact-title mapping of verified official NBS portal UUID eras.",
    period_policy = "january_not_reported_february_first_period",
    stringsAsFactors = FALSE
  )
}
datasets <- rbind(dataset_definition("industry"), dataset_definition("registration"))
bindings <- data.frame(
  manifest_version = version,
  route_type = "exact",
  dataset_id = dimensions$dataset_id,
  dimension_key = paste0(
    "series_id=",
    vapply(
      dimensions$value,
      utils::URLencode,
      character(1L),
      reserved = TRUE,
      USE.NAMES = FALSE
    )
  ),
  category_code = "",
  series_id = dimensions$value,
  stringsAsFactors = FALSE
)

connection <- gzfile(file.path(
  output_dir, "nbs-industrial-longitudinal-series-manifest.csv.gz"
), "wt")
utils::write.csv(long_manifest, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)
utils::write.csv(
  datasets,
  file.path(output_dir, "nbs-industrial-longitudinal-dataset-manifest.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
connection <- gzfile(file.path(
  output_dir, "nbs-industrial-longitudinal-dataset-dimensions.csv.gz"
), "wt")
utils::write.csv(dimensions, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)
connection <- gzfile(file.path(
  output_dir, "nbs-industrial-longitudinal-bindings.csv.gz"
), "wt")
utils::write.csv(bindings, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)

message(
  "Generated ", nrow(dimensions), " longitudinal industrial series from ",
  nrow(long_manifest), " exact official UUID-era slices."
)
