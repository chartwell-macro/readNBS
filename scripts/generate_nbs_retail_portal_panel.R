#!/usr/bin/env Rscript

# Generate the exact retail-release portal panel from the reviewed scalar
# manifest.  It deliberately exposes one NBS UUID leaf per tuple: released
# rows are not inferred from similarly named categories and no classification
# break is stitched by this script.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else file.path("inst", "extdata")
if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

version <- "2026-08-11.1"
dataset_id <- "NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING"
source_file <- file.path(output_dir, "nbs-series-manifest.csv")
if (!file.exists(source_file)) stop("Missing scalar manifest: ", source_file)

scalar <- utils::read.csv(
  source_file, stringsAsFactors = FALSE, check.names = FALSE,
  colClasses = "character", na.strings = character(), fileEncoding = "UTF-8"
)
scalar <- scalar[
  grepl("^NBS/CN/M/RETAIL/", scalar$series_id), , drop = FALSE
]
if (!nrow(scalar)) stop("The scalar manifest contains no retail leaves.")

# A small group of rows have multiple official transport slices.  The dataset
# member is the stable semantic series ID, so take its exact earliest and
# latest bounds while retaining that ID as the only selectable tuple.
ids <- unique(scalar$series_id)
members <- lapply(ids, function(id) {
  rows <- scalar[scalar$series_id == id, , drop = FALSE]
  first <- rows[which.min(as.Date(rows$slice_start)), , drop = FALSE]
  finite_end <- rows$slice_end[nzchar(rows$slice_end)]
  data.frame(
    series_id = id,
    label_en = first$title_en[[1L]],
    label_zh = first$title_zh[[1L]],
    valid_from = min(as.Date(rows$slice_start)),
    valid_to = if (length(finite_end) && length(finite_end) == nrow(rows)) {
      max(as.Date(finite_end))
    } else {
      as.Date(NA)
    },
    unit_en = first$unit_en[[1L]],
    unit_zh = first$unit_zh[[1L]],
    semantics = first$aggregation_basis[[1L]],
    stringsAsFactors = FALSE
  )
})
members <- do.call(rbind, members)
members <- members[order(members$series_id), , drop = FALSE]

dimension_key <- paste0(
  "series_id=", utils::URLencode(members$series_id, reserved = TRUE)
)
bindings <- data.frame(
  manifest_version = version,
  route_type = "exact",
  dataset_id = dataset_id,
  dimension_key = dimension_key,
  category_code = "",
  series_id = members$series_id,
  stringsAsFactors = FALSE
)
dimensions <- data.frame(
  manifest_version = version,
  dataset_id = dataset_id,
  dimension = "series_id",
  value = members$series_id,
  label_en = members$label_en,
  label_zh = members$label_zh,
  route_value = members$series_id,
  sort_order = seq_len(nrow(members)),
  valid_from = as.character(members$valid_from),
  valid_to = as.character(members$valid_to),
  unit_en = members$unit_en,
  unit_zh = members$unit_zh,
  semantics = members$semantics,
  stringsAsFactors = FALSE
)
dataset <- data.frame(
  manifest_version = version,
  dataset_id = dataset_id,
  release_id = "cn_nbs_activity",
  family = "retail_sales",
  title_en = "Retail sales — underlying official portal series",
  title_zh = "社会消费品零售总额—国家数据门户明细指标",
  description_en = paste(
    "Exact underlying NBS retail leaves: national total, urban/rural, goods,",
    "catering, above-designated enterprises, retail formats, online retail,",
    "and designated-goods categories. Select one exact published series ID."
  ),
  description_zh = paste(
    "国家统计局零售发布的精确底层指标：全国总额、城乡、商品、餐饮、",
    "限额以上单位、业态、网上零售和限额以上商品类别。请选择一个精确指标。"
  ),
  frequency = "M",
  route = "portal_uuid_panel",
  default_start = "2010-01-01",
  available_to = "",
  coverage_en = paste(nrow(members), "exact official retail portal leaves; exact member starts and calendar policies are retained"),
  coverage_zh = paste(nrow(members), "个国家统计局零售门户精确叶节点；保留每个指标的起始时间和发布日历"),
  required_dimensions = "series_id",
  panel_allowed = "TRUE",
  expected_rows = nrow(members),
  expected_cells = nrow(members),
  aggregation_basis = "mixed_span",
  seasonal_adjustment = "not_seasonally_adjusted",
  derived_method = "none",
  source_vintage = "latest_revised_database",
  series_note_en = paste(
    "Exact portal leaves only. NBS does not publish separate January/February",
    "current-month retail observations and omits January from cumulative series;",
    "the selected scalar leaf preserves its official policy."
  ),
  series_note_zh = paste(
    "仅包含精确门户叶节点。国家统计局不单独发布1月或2月当期零售观测，",
    "累计序列不发布1月；所选指标保留其官方日历规则。"
  ),
  unit_en = "Official published unit",
  unit_zh = "官方发布单位",
  source_url = "https://data.stats.gov.cn/",
  source_note = "Exact UUID bindings generated from the reviewed official NBS National Data portal manifest.",
  period_policy = "mixed_jan_feb_combined_then_monthly",
  stringsAsFactors = FALSE
)

write_csv <- function(x, filename) {
  utils::write.csv(x, file.path(output_dir, filename), row.names = FALSE,
    na = "", fileEncoding = "UTF-8")
}
write_csv(dataset, "nbs-retail-dataset-manifest.csv")
write_csv(dimensions, "nbs-retail-dataset-dimensions.csv")
write_csv(bindings, "nbs-retail-bindings.csv")
message("Wrote ", nrow(members), " exact retail portal tuples.")
