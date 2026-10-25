#!/usr/bin/env Rscript

# Build the reviewed macro-facing industrial/energy output panel from exact
# official NBS portal UUIDs. These are the gross-output indicators commonly
# used alongside headline industrial production.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else file.path("inst", "extdata")
if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

version <- "2026-08-18.2"
root_uuid <- "fc982599aa684be7969d7b90b1bd0e84"
dataset_id <- "NBS/CN/M/INDUSTRIAL_OUTPUT/KEY_PRODUCTS"
products <- data.frame(
  product = c("electricity", "cement", "plain_glass", "crude_steel", "rolled_steel"),
  product_en = c("Electricity", "Cement", "Plate glass", "Crude steel", "Steel products"),
  product_zh = c("发电量", "水泥", "平板玻璃", "粗钢", "钢材"),
  catalog_uuid = c(
    "1abb1cfea75847b8bf1a0e395d85966b",
    "266c750660954ea088db882a78103733",
    "dbe6f9d59d9a434793d53a027d5e93ef",
    "c72e324de29447e1a6917acd99c2d5ac",
    "a440d16b065c4c3b8c321378e56c2958"
  ),
  current_uuid = c(
    "baafe3a9a09d4b39a366e5b625574aea",
    "7bdff70c9a094d34b0250c4611312e38",
    "d96b26e5ecca4bc5b81c0b8663c2c806",
    "5f6643b26ae348baa1443f084cb3010e",
    "fe4e53f205d74b8e98f501b75cb27442"
  ),
  ytd_uuid = c(
    "a4c1c4e322c040488527169fb322844c",
    "290ebf582bbc4c57bbc388bce25fd978",
    "e1e420239e564e7d9527931105d99cad",
    "7849f12be5654f53b9aa2dee1880ba82",
    "b71c2705900d4e638fc226354c9ab409"
  ),
  current_yoy_uuid = c(
    "5cc686f383d84ed9bd11f4293ab4170e",
    "924d365b0a614637852fa5ae1369060d",
    "b1ad796684a045588ad8fa8431fdc3bf",
    "1e7dfadac47f4d04b403d8f4f7516c4b",
    "5c7138400d0143dc9ec83a4b82b55ad5"
  ),
  ytd_yoy_uuid = c(
    "619254b0b9f24a5aa0c336bdf5998c4e",
    "cff8b434fb7e49eda4b84562e9b9845d",
    "e0402f696e1f4cb08dc639237b3a28cf",
    "7b9be081b7ca4bfdb4c4518a045d8200",
    "7dd1397c70024406a04a071d1b2bb44c"
  ),
  level_unit_en = c(
    "100 million kWh", "10,000 tonnes", "10,000 weight cases",
    "10,000 tonnes", "10,000 tonnes"
  ),
  level_unit_zh = c("亿千瓦时", "万吨", "万重量箱", "万吨", "万吨"),
  stringsAsFactors = FALSE
)

measures <- data.frame(
  measure = c("current_level", "ytd_level", "current_yoy_percent", "ytd_yoy_percent"),
  uuid_column = c("current_uuid", "ytd_uuid", "current_yoy_uuid", "ytd_yoy_uuid"),
  label = c(
    "current output", "year-to-date output",
    "current year-on-year growth", "year-to-date year-on-year growth"
  ),
  policy = c(
    "current_january_february_not_separately_reported",
    "ytd_january_not_reported_february_combined",
    "current_january_february_not_separately_reported",
    "ytd_january_not_reported_february_combined"
  ),
  aggregation = c("single_period", "year_to_date", "single_period", "year_to_date"),
  start = c("2010-03-01", "2010-02-01", "2010-03-01", "2010-02-01"),
  stringsAsFactors = FALSE
)

rows <- list()
dimensions <- list()
for (product_index in seq_len(nrow(products))) {
  product <- products[product_index, ]
  for (measure_index in seq_len(nrow(measures))) {
    measure <- measures[measure_index, ]
    level <- measure$measure %in% c("current_level", "ytd_level")
    stable_id <- paste(
      "NBS", "CN", "M", "OUTPUT", toupper(product$product),
      toupper(measure$measure), sep = "/"
    )
    unit_en <- if (level) product$level_unit_en else "Percent"
    unit_zh <- if (level) product$level_unit_zh else "百分比"
    title_en <- paste(product$product_en, measure$label, sep = " — ")
    title_zh <- paste0(product$product_zh, "—", c(
      "当月产量", "累计产量", "当月同比增速", "累计同比增速"
    )[[measure_index]])
    indicator_uuid <- product[[measure$uuid_column]]
    rows[[length(rows) + 1L]] <- data.frame(
      manifest_version = version,
      series_id = stable_id,
      title_en = title_en,
      title_zh = title_zh,
      description_en = paste("Official NBS portal leaf:", title_en),
      description_zh = title_zh,
      frequency = "M",
      root_uuid = root_uuid,
      catalog_uuid = product$catalog_uuid,
      indicator_uuid = indicator_uuid,
      slice_start = measure$start,
      slice_end = "2026-06-01",
      default_start = measure$start,
      classification_id = "industrial-output-above-designated-size",
      stitch_policy = "single_continuous_leaf",
      period_policy = measure$policy,
      aggregation_basis = measure$aggregation,
      seasonal_adjustment = "not_seasonally_adjusted",
      series_note_en = paste(
        "Official output of enterprises above designated size. January and",
        "February are not separately reported for current-period output; the",
        "cumulative February value covers both months."
      ),
      series_note_zh = "规模以上工业企业官方产量；当月产量不单独发布一月、二月，二月累计值涵盖前两个月。",
      unit_en = unit_en,
      unit_zh = unit_zh,
      scale = "1",
      geography_code = "000000000000",
      geography_en = "China",
      geography_zh = "中国",
      source_url = paste0(
        "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid?cid=",
        product$catalog_uuid
      ),
      source_note = "Latest/revised NBS National Data portal; exact UUID verified 2026-08-18.",
      stringsAsFactors = FALSE
    )
    dimensions[[length(dimensions) + 1L]] <- data.frame(
      manifest_version = version,
      dataset_id = dataset_id,
      dimension = "series_id",
      value = stable_id,
      label_en = title_en,
      label_zh = title_zh,
      route_value = stable_id,
      sort_order = (product_index - 1L) * nrow(measures) + measure_index,
      valid_from = measure$start,
      valid_to = "2026-06-01",
      unit_en = unit_en,
      unit_zh = unit_zh,
      semantics = if (measure$aggregation == "year_to_date") "year_to_date" else "single_period",
      stringsAsFactors = FALSE
    )
  }
}
manifest <- do.call(rbind, rows)
dimensions <- do.call(rbind, dimensions)
dataset <- data.frame(
  manifest_version = version,
  dataset_id = dataset_id,
  release_id = "cn_nbs_activity",
  family = "industrial_output",
  title_en = "Key industrial and energy output — long series",
  title_zh = "主要工业与能源产量—长序列",
  description_en = paste(
    "Electricity, cement, plate glass, crude steel and steel products, with",
    "current, cumulative and year-on-year measures from 2010."
  ),
  description_zh = "2010年以来发电量、水泥、平板玻璃、粗钢和钢材的当月、累计及同比指标。",
  frequency = "M",
  route = "portal_uuid_panel",
  default_start = "2010-02-01",
  available_to = "2026-06-01",
  coverage_en = "5 key products x 4 exact official measures; 164 to 181 reported monthly observations per series",
  coverage_zh = "5种主要产品×4项官方精确指标；每序列164至181个月度观测",
  required_dimensions = "series_id",
  panel_allowed = "TRUE",
  expected_rows = nrow(dimensions),
  expected_cells = nrow(dimensions),
  aggregation_basis = "mixed_span",
  seasonal_adjustment = "not_seasonally_adjusted",
  derived_method = "none",
  source_vintage = "latest_revised_database",
  series_note_en = paste(
    "Current-period values begin in March because NBS reports January and",
    "February together; cumulative values begin with the combined February period."
  ),
  series_note_zh = "当月值始于三月，因为国家统计局合并发布一月和二月；累计值始于合并的一至二月期。",
  unit_en = "Official published unit",
  unit_zh = "官方发布单位",
  source_url = "https://data.stats.gov.cn/",
  source_note = "Exact UUID bindings from the official NBS National Data portal.",
  period_policy = "mixed_jan_feb_combined_then_monthly",
  stringsAsFactors = FALSE
)
bindings <- data.frame(
  manifest_version = version,
  route_type = "exact",
  dataset_id = dataset_id,
  dimension_key = paste0(
    "series_id=",
    vapply(dimensions$value, utils::URLencode, character(1L),
      reserved = TRUE, USE.NAMES = FALSE)
  ),
  category_code = "",
  series_id = dimensions$value,
  stringsAsFactors = FALSE
)

utils::write.csv(manifest, file.path(output_dir, "nbs-key-output-series-manifest.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
utils::write.csv(dataset, file.path(output_dir, "nbs-key-output-dataset-manifest.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
utils::write.csv(dimensions, file.path(output_dir, "nbs-key-output-dataset-dimensions.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
utils::write.csv(bindings, file.path(output_dir, "nbs-key-output-bindings.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Generated ", nrow(manifest), " key industrial/energy output series.")
