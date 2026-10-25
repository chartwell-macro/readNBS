#!/usr/bin/env Rscript

# Build the retained 2011--12 annual urban and rural household-survey panels.
#
# These two Yearbook vintages precede the integrated household survey.  They
# must remain separate from NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION, which
# begins with the later unified survey.  The script is deliberately offline:
# it parses only the official raw captures retained under inst/extdata.

suppressPackageStartupMessages({
  library(digest)
  library(readxl)
  library(rvest)
  library(xml2)
})

source("R/yearbook.R", local = TRUE)

root <- "inst/extdata"
raw_root <- file.path(root, "nbs-households-legacy")
version <- "2026-08-11.1"

urban <- data.frame(
  row_2011 = c(5L, 6L, 7L, 9L, 11L, 12:27, 30:37),
  row_2012 = c(5L, 6L, 7L, 9L, 11L, 12:27, 30:37),
  series = c(
    "survey_households", "household_size", "employed_persons",
    "employment_rate", "dependency_ratio", "annual_income_total",
    "annual_income_wages", "annual_income_business_net",
    "annual_income_property", "annual_income_transfers",
    "disposable_income", "annual_expenditure_total",
    "cash_consumption_total", "cash_consumption_food",
    "cash_consumption_clothing", "cash_consumption_housing",
    "cash_consumption_household_goods_services",
    "cash_consumption_transport_communications",
    "cash_consumption_education_culture_recreation",
    "cash_consumption_health_care", "cash_consumption_other",
    "cash_consumption_share_food", "cash_consumption_share_clothing",
    "cash_consumption_share_housing",
    "cash_consumption_share_household_goods_services",
    "cash_consumption_share_transport_communications",
    "cash_consumption_share_education_culture_recreation",
    "cash_consumption_share_health_care", "cash_consumption_share_other"
  ),
  label_en = c(
    "Survey households", "Average household size", "Employed persons per household",
    "Employment rate per household", "Dependency ratio per employed person",
    "Per-capita annual income", "Wage income", "Net business income",
    "Property income", "Transfer income", "Per-capita disposable income",
    "Per-capita total expenditure", "Per-capita cash consumption expenditure",
    "Cash consumption: food", "Cash consumption: clothing", "Cash consumption: housing",
    "Cash consumption: household goods and services",
    "Cash consumption: transport and communications",
    "Cash consumption: education, culture and recreation",
    "Cash consumption: health care", "Cash consumption: other",
    "Cash-consumption share: food", "Cash-consumption share: clothing",
    "Cash-consumption share: housing",
    "Cash-consumption share: household goods and services",
    "Cash-consumption share: transport and communications",
    "Cash-consumption share: education, culture and recreation",
    "Cash-consumption share: health care", "Cash-consumption share: other"
  ),
  label_zh = c(
    "调查户数", "平均每户家庭人口", "平均每户就业人口", "平均每户就业面",
    "平均每一就业者负担人数（包括就业者本人）", "平均每人全部年收入", "工资性收入",
    "经营净收入", "财产性收入", "转移性收入", "可支配收入", "平均每人总支出",
    "平均每人现金消费支出", "食品", "衣着", "居住", "家庭设备及用品", "交通通信",
    "文教娱乐", "医疗保健", "其他", "食品占比", "衣着占比", "居住占比",
    "家庭设备及用品占比", "交通通信占比", "文教娱乐占比", "医疗保健占比", "其他占比"
  ),
  unit_en = c("Households", "Persons", "Persons", "Percent", "Persons",
              rep("Yuan", 16L), rep("Percent", 8L)),
  unit_zh = c("户", "人", "人", "百分比", "人",
              rep("元", 16L), rep("百分比", 8L)),
  stringsAsFactors = FALSE
)

rural <- data.frame(
  row_2011 = c(5L, 7L, 8L, 10L, 12:26, 28:41),
  row_2012 = c(5L, 7L, 9L, 12L, 14:28, 30:43),
  series = c(
    "survey_households", "resident_persons_per_household", "labour_units_per_household",
    "dependency_ratio", "annual_income_total", "annual_income_wages",
    "annual_income_household_business", "annual_income_property",
    "annual_income_transfers", "cash_income_total", "cash_income_wages",
    "cash_income_household_business", "cash_income_property", "cash_income_transfers",
    "net_income_total", "net_income_wages", "net_income_household_business",
    "net_income_property", "net_income_transfers", "annual_expenditure_total",
    "annual_expenditure_household_business", "annual_expenditure_productive_assets",
    "annual_expenditure_taxes_fees", "annual_expenditure_consumption",
    "annual_expenditure_property", "annual_expenditure_transfers", "cash_expenditure_total",
    "cash_expenditure_household_business", "cash_expenditure_productive_assets",
    "cash_expenditure_taxes_fees", "cash_expenditure_consumption",
    "cash_expenditure_property", "cash_expenditure_transfers"
  ),
  label_en = c(
    "Survey households", "Usual resident persons per household", "Labour units per household",
    "Dependency ratio per labour unit", "Per-capita annual income", "Wage income",
    "Household-business income", "Property income", "Transfer income", "Per-capita cash income",
    "Cash wage income", "Cash household-business income", "Cash property income",
    "Cash transfer income", "Per-capita net income", "Net wage income",
    "Net household-business income", "Net property income", "Net transfer income",
    "Per-capita annual expenditure", "Household-business expenditure",
    "Productive fixed-asset purchases", "Taxes and fees", "Consumption expenditure",
    "Property expenditure", "Transfer expenditure", "Per-capita cash expenditure",
    "Cash household-business expenditure", "Cash productive fixed-asset purchases",
    "Cash taxes and fees", "Cash consumption expenditure", "Cash property expenditure",
    "Cash transfer expenditure"
  ),
  label_zh = c(
    "调查户数", "平均每户常住人口", "平均每户整半劳动力", "平均每个劳动力负担人口（含本人）",
    "平均每人年收入：总收入", "平均每人年收入：工资性收入", "平均每人年收入：家庭经营收入",
    "平均每人年收入：财产性收入", "平均每人年收入：转移性收入", "现金收入",
    "现金收入：工资性收入", "现金收入：家庭经营收入", "现金收入：财产性收入",
    "现金收入：转移性收入", "纯收入", "纯收入：工资性收入", "纯收入：家庭经营纯收入",
    "纯收入：财产性收入", "纯收入：转移性收入", "平均每人年支出：总支出",
    "平均每人年支出：家庭经营费用支出", "平均每人年支出：购置生产性固定资产",
    "平均每人年支出：税费支出", "平均每人年支出：消费支出", "平均每人年支出：财产性支出",
    "平均每人年支出：转移性支出", "现金支出", "现金支出：家庭经营费用支出",
    "现金支出：购买生产性固定资产", "现金支出：税费支出", "现金支出：消费支出",
    "现金支出：财产性支出", "现金支出：转移性支出"
  ),
  unit_en = c("Households", rep("Persons", 3L), rep("Yuan", 29L)),
  unit_zh = c("户", rep("人", 3L), rep("元", 29L)),
  stringsAsFactors = FALSE
)

stopifnot(nrow(urban) == 29L, nrow(rural) == 33L,
          !anyDuplicated(urban$series), !anyDuplicated(rural$series))

.read_2011_html <- function(path, rows, year = 2011L) {
  raw <- readBin(path, "raw", file.info(path)$size)
  html <- .nbs_yearbook_decode_html(raw)
  document <- xml2::read_html(html)
  tables <- rvest::html_elements(document, "table")
  if (length(tables) > 1L) xml2::xml_remove(tables[-1L])
  grid <- .nbs_yearbook_table_grid(as.character(document), path)$values
  values <- grid[rows, ncol(grid)]
  if (!identical(trimws(grid[[3L, ncol(grid)]]), as.character(year)) ||
      any(!.nbs_yearbook_is_number(values))) {
    stop("Unexpected 2011 household-table geometry or values: ", path, call. = FALSE)
  }
  list(raw = raw, values = values, source_labels = grid[rows, 1L])
}

.read_2012_xls <- function(path, rows, year = 2012L) {
  sheet <- readxl::read_excel(path, col_names = FALSE, .name_repair = "minimal")
  candidate <- which(!vapply(sheet, is.logical, logical(1L)))
  column <- max(candidate)
  values <- sheet[[column]][rows]
  if (!identical(as.character(sheet[[column]][[3L]]), as.character(year)) ||
      any(!is.finite(values))) {
    stop("Unexpected 2012 household-table geometry or values: ", path, call. = FALSE)
  }
  list(
    raw = readBin(path, "raw", file.info(path)$size),
    values = format(values, scientific = FALSE, trim = TRUE, digits = 15L),
    source_labels = as.character(sheet[[1L]][rows])
  )
}

.build_panel <- function(definition, source_rows, residence, year, path, reader, dataset_id,
                         source_table, source_url) {
  parsed <- reader(path, source_rows, year)
  raw_value <- trimws(as.character(parsed$values))
  if (any(!.nbs_yearbook_is_number(raw_value))) {
    stop("A retained legacy observation is not numeric.", call. = FALSE)
  }
  data.frame(
    manifest_version = version,
    dataset_id = dataset_id,
    release_period = as.character(year),
    category = "", industry = "", city_code = "", city_label = "", market = "",
    size_band = "", comparison = "", index_base = "", region = "",
    measure = "value", period_basis = "annual", residence = residence,
    nominal_real = "nominal", unit_type = "", registration = "", occupation = "",
    product_code = "", specification = "", unit = definition$unit_en,
    section = paste0("household_survey_", residence),
    row_label = definition$label_zh, table_index = 1L, row_index = source_rows,
    column_index = 1L, raw_value = raw_value, value = as.numeric(gsub(",", "", raw_value)),
    observation_date = sprintf("%d-01-01", year), series = definition$series,
    reported_observation_date = sprintf("%d-01-01", year),
    observation_period_start = sprintf("%d-01-01", year),
    observation_period_end = sprintf("%d-12-31", year),
    comparison_period_start = "", comparison_period_end = "", comparison_basis = "",
    published_value_type = "level", schema_version = "pre_unified_household_survey",
    basket_version = "", classification_id = paste0(residence, "_household_survey_pre2013"),
    coverage_version = "annual_legacy_survey", seasonal_adjustment = "not_seasonally_adjusted",
    dimension_label_en = definition$label_en, dimension_label_zh = definition$label_zh,
    unit_en = definition$unit_en, unit_zh = definition$unit_zh,
    source_table = source_table, source_url = source_url,
    source_capture_sha256 = digest::digest(parsed$raw, algo = "sha256", serialize = FALSE),
    stringsAsFactors = FALSE
  )
}

datasets <- data.frame(
  dataset_id = c(
    "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
    "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012"
  ),
  residence = c("urban", "rural"),
  source_table_2011 = c("10-5", "10-18"),
  source_table_2012 = c("11-4", "11-17"),
  source_path_2011 = c("2012/J1005C.HTM", "2012/J1018C.HTM"),
  source_path_2012 = c("2013/Z1104C.xls", "2013/Z1117C.xls"),
  source_url_2011 = c(
    "https://www.stats.gov.cn/sj/ndsj/2012/html/J1005C.HTM",
    "https://www.stats.gov.cn/sj/ndsj/2012/html/J1018C.HTM"
  ),
  source_url_2012 = c(
    "https://www.stats.gov.cn/sj/ndsj/2013/html/Z1104C.xls",
    "https://www.stats.gov.cn/sj/ndsj/2013/html/Z1117C.xls"
  ),
  stringsAsFactors = FALSE
)

parts <- list()
for (index in seq_len(nrow(datasets))) {
  item <- datasets[index, , drop = FALSE]
  definition <- if (item$residence[[1L]] == "urban") urban else rural
  parts[[length(parts) + 1L]] <- .build_panel(
    definition, definition$row_2011, item$residence[[1L]], 2011L,
    file.path(raw_root, item$source_path_2011[[1L]]), .read_2011_html,
    item$dataset_id[[1L]], item$source_table_2011[[1L]], item$source_url_2011[[1L]]
  )
  parts[[length(parts) + 1L]] <- .build_panel(
    definition, definition$row_2012, item$residence[[1L]], 2012L,
    file.path(raw_root, item$source_path_2012[[1L]]), .read_2012_xls,
    item$dataset_id[[1L]], item$source_table_2012[[1L]], item$source_url_2012[[1L]]
  )
}
observations <- do.call(rbind, parts)
if (anyDuplicated(paste(observations$dataset_id, observations$release_period,
                        observations$series, sep = "\r"))) {
  stop("Legacy household observations are not unique.", call. = FALSE)
}

archive_path <- file.path(root, "nbs-households-legacy-observations.csv.gz")
archive_connection <- gzfile(archive_path, "wb")
utils::write.csv(observations, archive_connection, row.names = FALSE,
                 na = "", fileEncoding = "UTF-8")
close(archive_connection)
archive_sha <- digest::digest(file = archive_path, algo = "sha256", serialize = FALSE)

release_index <- do.call(rbind, lapply(seq_len(nrow(datasets)), function(index) {
  item <- datasets[index, , drop = FALSE]
  definition <- if (item$residence[[1L]] == "urban") urban else rural
  data.frame(
    manifest_version = version, dataset_id = item$dataset_id[[1L]],
    period = c("2011", "2012"), reference_period_start = c("2011-01-01", "2012-01-01"),
    reference_period_end = c("2011-12-31", "2012-12-31"),
    release_date = c("2012-09-03", "2013-09-03"),
    source_url = c(item$source_url_2011[[1L]], item$source_url_2012[[1L]]),
    basket_version = "", index_base = "", expected_press_cells = nrow(definition),
    schema_version = paste0(item$residence[[1L]], "_household_survey_pre2013"),
    storage = "snapshot", snapshot_sha256 = archive_sha,
    source_capture_sha256 = observations$source_capture_sha256[
      match(paste(item$dataset_id[[1L]], c("2011", "2012"), sep = "\r"),
            paste(observations$dataset_id, observations$release_period, sep = "\r"))
    ],
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(release_index, file.path(root, "nbs-households-legacy-release-index.csv"),
                 row.names = FALSE, na = "", fileEncoding = "UTF-8")

manifest <- do.call(rbind, lapply(seq_len(nrow(datasets)), function(index) {
  item <- datasets[index, , drop = FALSE]
  definition <- if (item$residence[[1L]] == "urban") urban else rural
  title <- if (item$residence[[1L]] == "urban") {
    c("Annual urban household survey detail (2011-12 legacy)", "2011—2012年城镇居民家庭年度调查明细（历史口径）")
  } else {
    c("Annual rural household survey detail (2011-12 legacy)", "2011—2012年农村居民家庭年度调查明细（历史口径）")
  }
  data.frame(
    manifest_version = version, dataset_id = item$dataset_id[[1L]],
    release_id = "cn_nbs_households_legacy_yearbook", family = "households_legacy",
    title_en = title[[1L]], title_zh = title[[2L]],
    description_en = paste0(nrow(definition), " exact annual pre-unified-survey household measures for 2011 and 2012."),
    description_zh = paste0("2011年和2012年统一住户调查前的", nrow(definition), "项精确年度住户指标。"),
    frequency = "A", route = "press_release_table", default_start = "2011-01-01",
    available_to = "2012-12-31",
    coverage_en = paste0(nrow(definition), " exact measures × two annual Yearbook vintages; not spliced into the 2013+ unified quarterly survey."),
    coverage_zh = paste0(nrow(definition), "项精确指标 × 两个年度年鉴版本；不与2013年后的统一住户季度调查拼接。"),
    required_dimensions = "series", panel_allowed = "FALSE", expected_rows = nrow(definition),
    expected_cells = nrow(definition), aggregation_basis = "single_period",
    seasonal_adjustment = "not_seasonally_adjusted", derived_method = "none",
    source_vintage = "retained_official_yearbook_legacy_snapshot",
    series_note_en = "These legacy urban/rural survey definitions are a separate annual methodology and are not conceptually continuous with the 2013+ integrated household survey.",
    series_note_zh = "历史城乡住户调查为独立年度口径，不与2013年后的统一住户调查在概念上连续拼接。",
    unit_en = "Measure-specific", unit_zh = "指标对应单位",
    source_url = item$source_url_2012[[1L]],
    source_note = "Retained official Yearbook source captures with SHA-256 provenance; offline exact snapshot.",
    period_policy = "all_periods", stringsAsFactors = FALSE
  )
}))
utils::write.csv(manifest, file.path(root, "nbs-households-legacy-dataset-manifest.csv"),
                 row.names = FALSE, na = "", fileEncoding = "UTF-8")

dimension_rows <- do.call(rbind, lapply(seq_len(nrow(datasets)), function(index) {
  item <- datasets[index, , drop = FALSE]
  definition <- if (item$residence[[1L]] == "urban") urban else rural
  data.frame(
    manifest_version = version, dataset_id = item$dataset_id[[1L]], dimension = "series",
    value = definition$series, label_en = definition$label_en, label_zh = definition$label_zh,
    route_value = definition$series, sort_order = seq_len(nrow(definition)),
    valid_from = "2011-01-01", valid_to = "2012-12-31",
    unit_en = definition$unit_en, unit_zh = definition$unit_zh,
    semantics = "annual_pre_unified_household_survey", stringsAsFactors = FALSE
  )
}))
utils::write.csv(dimension_rows, file.path(root, "nbs-households-legacy-dataset-dimensions.csv"),
                 row.names = FALSE, na = "", fileEncoding = "UTF-8")

source_index <- observations[, c("dataset_id", "release_period", "residence", "source_table",
                                  "source_url", "source_capture_sha256")]
source_index <- unique(source_index)
utils::write.csv(source_index, file.path(root, "nbs-households-legacy-source-index.csv"),
                 row.names = FALSE, na = "", fileEncoding = "UTF-8")

message(sprintf("Wrote %d observations (%d urban, %d rural); archive SHA-256 %s.",
  nrow(observations), sum(observations$residence == "urban"),
  sum(observations$residence == "rural"), archive_sha))
