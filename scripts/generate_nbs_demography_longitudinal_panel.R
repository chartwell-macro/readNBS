#!/usr/bin/env Rscript

# Build the chart-ready population panel from exact official NBS portal
# leaves. The communiqué tables remain a first-release archive; this panel is
# the ordinary longitudinal, latest-revised database view.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else file.path("inst", "extdata")
if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

version <- "2026-08-18.2"
dataset_id <- "NBS/CN/A/DEMOGRAPHY/LONGITUDINAL"
manifest <- utils::read.csv(
  file.path(output_dir, "nbs-series-manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  colClasses = "character",
  na.strings = character(),
  fileEncoding = "UTF-8"
)
members <- manifest[startsWith(manifest$series_id, "NBS/CN/A/POPULATION/"), ]
members <- members[order(as.Date(members$slice_start), members$series_id), ]

if (nrow(members) != 14L || anyDuplicated(members$series_id) ||
    any(members$frequency != "A") || any(members$aggregation_basis != "single_period")) {
  stop("The reviewed population portal member set has changed.")
}

dataset <- data.frame(
  manifest_version = version,
  dataset_id = dataset_id,
  release_id = "cn_nbs_demographics",
  family = "demographics",
  title_en = "Population and demographics — longitudinal series",
  title_zh = "人口与人口结构—纵向序列",
  description_en = paste(
    "Long official annual histories for population totals, age structure,",
    "sex, urban/rural residence, vital rates and dependency ratios."
  ),
  description_zh = "人口总量、年龄结构、性别、城乡、人口变动率及抚养比的官方年度长序列。",
  frequency = "A",
  route = "portal_uuid_panel",
  default_start = min(members$slice_start),
  available_to = "",
  coverage_en = "14 exact official annual portal series; total population begins in 1949 and the detailed panel in 2000",
  coverage_zh = "14项国家统计局年度门户精确序列；总人口始于1949年，详细面板始于2000年",
  required_dimensions = "series_id",
  panel_allowed = "TRUE",
  expected_rows = nrow(members),
  expected_cells = nrow(members),
  aggregation_basis = "single_period",
  seasonal_adjustment = "not_seasonally_adjusted",
  derived_method = "none",
  source_vintage = "latest_revised_database",
  series_note_en = paste(
    "Chart-ready latest-revised portal histories. Exact age definitions are",
    "kept as separate series; no incompatible age bands are spliced."
  ),
  series_note_zh = "用于制图的门户最新修订历史；不同年龄定义保持为独立序列，不作不相容拼接。",
  unit_en = "Official published unit",
  unit_zh = "官方发布单位",
  source_url = "https://data.stats.gov.cn/",
  source_note = "Exact UUID bindings from the reviewed official NBS National Data portal manifest.",
  period_policy = "all_periods",
  stringsAsFactors = FALSE
)

dimensions <- data.frame(
  manifest_version = version,
  dataset_id = dataset_id,
  dimension = "series_id",
  value = members$series_id,
  label_en = trimws(members$title_en),
  label_zh = trimws(members$title_zh),
  route_value = members$series_id,
  sort_order = seq_len(nrow(members)),
  valid_from = members$slice_start,
  valid_to = members$slice_end,
  unit_en = members$unit_en,
  unit_zh = members$unit_zh,
  semantics = "single_period",
  stringsAsFactors = FALSE
)
bindings <- data.frame(
  manifest_version = version,
  route_type = "exact",
  dataset_id = dataset_id,
  dimension_key = paste0(
    "series_id=",
    vapply(
      members$series_id,
      utils::URLencode,
      character(1L),
      reserved = TRUE,
      USE.NAMES = FALSE
    )
  ),
  category_code = "",
  series_id = members$series_id,
  stringsAsFactors = FALSE
)

utils::write.csv(
  dataset,
  file.path(output_dir, "nbs-demography-longitudinal-dataset-manifest.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
utils::write.csv(
  dimensions,
  file.path(output_dir, "nbs-demography-longitudinal-dataset-dimensions.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
utils::write.csv(
  bindings,
  file.path(output_dir, "nbs-demography-longitudinal-bindings.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
message("Generated ", nrow(members), " longitudinal population series.")
