#!/usr/bin/env Rscript

# Build the retained 2010--21 annual wage-history supplement from the
# authoritative NBS Chinese release pages.  These pages pre-date the modern
# eight-table release.  They are consequently exposed as two source-faithful
# panels (non-private and private) and require an explicit industry-standard
# era when fetched.  We retain current-year level and published nominal growth;
# the repeated prior-year level is deliberately not duplicated as a second
# vintage of the same annual observation.

suppressPackageStartupMessages({
  library(curl)
  library(digest)
  library(rvest)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
root <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(root, "inst", "extdata"))) {
  stop("Run this builder from the readNBS repository root.", call. = FALSE)
}

download <- "--download" %in% commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
manifest_version <- "2026-08-11.1"
dataset_for <- c(
  non_private = "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY",
  private = "NBS/CN/A/WAGES/PRIVATE_HISTORY"
)

source_rows <- do.call(rbind, lapply(2010:2021, function(year) {
  suffix <- switch(
    as.character(year),
    "2010" = c("202303/t20230301_1919259", "202303/t20230301_1919260"),
    "2011" = c("202303/t20230301_1919068", "202303/t20230301_1919069"),
    "2012" = c("202302/t20230203_1898302", "202302/t20230203_1898301"),
    "2013" = c("202302/t20230203_1898520", "202302/t20230203_1898519"),
    "2014" = c("202302/t20230203_1898795", "202302/t20230203_1898796"),
    "2015" = c("202302/t20230203_1899114", "202302/t20230203_1899115"),
    "2016" = c("202302/t20230203_1899525", "202302/t20230203_1899526"),
    "2017" = c("202302/t20230203_1899943", "202302/t20230203_1899944"),
    "2018" = c("202302/t20230203_1900311", "202302/t20230203_1900312"),
    "2019" = c("202302/t20230203_1900727", "202302/t20230203_1900726"),
    "2020" = c("202302/t20230203_1901108", "202302/t20230203_1901109"),
    "2021" = c("202302/t20230203_1901472", "202302/t20230203_1901473")
  )
  data.frame(
    year = year,
    unit_type = c("non_private", "private"),
    source_url = paste0("https://www.stats.gov.cn/sj/zxfb/", suffix, ".html"),
    stringsAsFactors = FALSE
  )
}))

raw_dir <- file.path(root, "inst", "extdata", "nbs-wages-history")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
for (i in seq_len(nrow(source_rows))) {
  row <- source_rows[i, ]
  path <- file.path(raw_dir, sprintf("%d-%s.html", row$year, row$unit_type))
  if (!file.exists(path) || (download && refresh)) {
    response <- curl_fetch_memory(row$source_url, handle = new_handle(followlocation = TRUE))
    if (response$status_code != 200L || !length(response$content)) {
      stop("Could not download official NBS wage source: ", row$source_url, call. = FALSE)
    }
    writeBin(response$content, path)
  }
  source_rows$raw_path[i] <- path
  source_rows$source_capture_sha256[i] <- digest(file = path, algo = "sha256", serialize = FALSE)
}

normalize_label <- function(x) {
  x <- enc2utf8(trimws(as.character(x)))
  x <- gsub("[[:space:]　 ]", "", x, perl = TRUE)
  x <- gsub("[、，,（）()]", "", x, perl = TRUE)
  x
}

maps <- list(
  region = c(
    total = "Total", eastern = "Eastern", central = "Central",
    western = "Western", northeastern = "Northeastern"
  ),
  industry = c(
    total = "Total",
    agriculture_forestry_animal_husbandry_and_fishery = "Agriculture, forestry, animal husbandry and fishery",
    mining = "Mining", manufacturing = "Manufacturing",
    production_and_supply_of_electricity_heat_power_gas_and_water = "Electricity, heat, gas and water supply",
    construction = "Construction", wholesale_and_retail_trades = "Wholesale and retail trades",
    transport_storage_and_post = "Transport, storage and post",
    hotels_and_catering_services = "Accommodation and catering",
    information_transmission_software_and_information_technology_services = "Information transmission, software and IT services",
    finance = "Finance", real_estate = "Real estate",
    leasing_and_business_services = "Leasing and business services",
    scientific_research_and_technical_services = "Scientific research and technical services",
    management_of_water_conservancy_environment_and_public_facilities = "Water, environment and public facilities management",
    household_services_repair_and_other_services = "Household, repair and other services",
    education = "Education", health_and_social_services = "Health and social work",
    culture_sports_and_entertainment = "Culture, sports and entertainment",
    public_management_social_security_and_social_organizations = "Public management, social security and social organisations"
  ),
  registration = c(
    total = "Total", state_owned = "State-owned", urban_collective = "Urban collective",
    shareholding_cooperative = "Shareholding cooperative", joint_ownership = "Joint ownership",
    limited_liability_company = "Limited-liability company", shareholding_company = "Shareholding company",
    other_domestic = "Other domestic", hong_kong_macao_taiwan_invested = "Hong Kong, Macao and Taiwan invested",
    foreign_invested = "Foreign invested", state_holding = "State holding"
  )
)

map_region <- function(x) {
  y <- normalize_label(x)
  if (grepl("^(合计|全部|全国)", y)) return("total")
  if (grepl("东部", y)) return("eastern")
  if (grepl("中部", y)) return("central")
  if (grepl("西部", y)) return("western")
  if (grepl("东北", y)) return("northeastern")
  provinces <- c(
    beijing = "北京", tianjin = "天津", hebei = "河北", shanxi = "山西",
    inner_mongolia = "内蒙古", liaoning = "辽宁", jilin = "吉林", heilongjiang = "黑龙江",
    shanghai = "上海", jiangsu = "江苏", zhejiang = "浙江", anhui = "安徽",
    fujian = "福建", jiangxi = "江西", shandong = "山东", henan = "河南",
    hubei = "湖北", hunan = "湖南", guangdong = "广东", guangxi = "广西",
    hainan = "海南", chongqing = "重庆", sichuan = "四川", guizhou = "贵州",
    yunnan = "云南", tibet = "西藏", shaanxi = "陕西", gansu = "甘肃",
    qinghai = "青海", ningxia = "宁夏", xinjiang = "新疆"
  )
  hit <- names(provinces)[vapply(provinces, grepl, logical(1), x = y, fixed = TRUE)]
  if (length(hit) == 1L) return(hit)
  NA_character_
}
map_industry <- function(x) {
  y <- normalize_label(x)
  if (grepl("^(合计|全部)$", y)) return("total")
  cases <- c(
    agriculture_forestry_animal_husbandry_and_fishery = "农.*林.*牧.*渔",
    mining = "采矿", manufacturing = "制造",
    production_and_supply_of_electricity_heat_power_gas_and_water = "电力.*(热.*)?燃气.*水",
    construction = "建筑", wholesale_and_retail_trades = "批发.*零售",
    transport_storage_and_post = "交通运输.*仓储.*邮政",
    hotels_and_catering_services = "住宿.*餐饮",
    information_transmission_software_and_information_technology_services = "信息传输.*软件.*信息技术",
    finance = "金融", real_estate = "房地",
    leasing_and_business_services = "租赁.*商务",
    scientific_research_and_technical_services = "科学研究.*技术",
    management_of_water_conservancy_environment_and_public_facilities = "水利.*环境.*公共设施",
    household_services_repair_and_other_services = "居民服务.*修理.*其他服务",
    education = "教育", health_and_social_services = "卫生.*社会工作",
    culture_sports_and_entertainment = "文化.*体育.*娱乐",
    public_management_social_security_and_social_organizations = "公共管理.*社会保障.*社会组织"
  )
  hit <- names(cases)[vapply(cases, grepl, logical(1), x = y, perl = TRUE)]
  if (length(hit) == 1L) hit else NA_character_
}
map_registration <- function(x) {
  y <- normalize_label(x)
  if (grepl("^(合计|全部)$", y)) return("total")
  cases <- c(
    state_holding = "国有控股", state_owned = "国有",
    urban_collective = "城镇集体|集体单位", shareholding_cooperative = "股份合作",
    joint_ownership = "联营", limited_liability_company = "有限责任",
    shareholding_company = "股份有限|股份公司", other_domestic = "其他内资|其他",
    hong_kong_macao_taiwan_invested = "港澳台", foreign_invested = "外商"
  )
  hit <- names(cases)[vapply(cases, grepl, logical(1), x = y, perl = TRUE)]
  if (length(hit) == 1L) hit else NA_character_
}

classification_for <- function(year, unit_type) {
  if (year <= 2011L) {
    paste0(unit_type, "_employees_gbt4754_2002")
  } else if (year <= 2017L) {
    "gbt4754_2011"
  } else {
    "gbt4754_2017"
  }
}

release_date <- function(html, year) {
  text <- html_text2(read_html(html))
  hit <- regmatches(text, regexpr("20[0-9]{2}[/-][0-9]{1,2}[/-][0-9]{1,2}", text, perl = TRUE))
  if (length(hit) && nzchar(hit)) {
    parts <- as.integer(unlist(strsplit(gsub("/", "-", hit), "-", fixed = TRUE)))
    if (length(parts) == 3L && parts[[1L]] %in% c(year, year + 1L)) {
      return(sprintf("%04d-%02d-%02d", parts[[1L]], parts[[2L]], parts[[3L]]))
    }
  }
  sprintf("%04d-05-01", year + 1L)
}

numeric_value <- function(x) {
  x <- gsub("[[:space:],，]", "", as.character(x), perl = TRUE)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", x)) return(NA_real_)
  as.numeric(x)
}

empty_record <- function() {
  as.list(setNames(rep("", 45L), c(
    "manifest_version", "dataset_id", "release_period", "category", "industry",
    "city_code", "city_label", "market", "size_band", "comparison", "index_base",
    "region", "measure", "period_basis", "residence", "nominal_real", "unit_type",
    "registration", "occupation", "product_code", "specification", "unit", "section",
    "row_label", "table_index", "row_index", "column_index", "raw_value", "value",
    "observation_date", "series_id", "reported_observation_date", "observation_period_start",
    "observation_period_end", "comparison_period_start", "comparison_period_end",
    "comparison_basis", "published_value_type", "schema_version", "basket_version",
    "classification_id", "coverage_version", "seasonal_adjustment", "release_source_url"
  )))
}

parse_one <- function(row) {
  doc <- read_html(row$raw_path[[1L]])
  tables <- html_table(doc, fill = TRUE, convert = FALSE)
  signature <- vapply(tables, function(x) paste(as.matrix(x), collapse = "\r"), character(1))
  tables <- tables[!duplicated(signature)]
  records <- list()
  for (table_index in seq_along(tables)) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    text <- apply(table, 1L, function(z) paste(z, collapse = " "))
    normalized_text <- normalize_label(text)
    header_candidates <- which(grepl("地区|区域|行业|登记注册", normalized_text) &
      vapply(seq_len(nrow(table)), function(j) {
        sum(grepl(paste0(row$year, "年"), table[j, ])) == 1L
      }, logical(1)))
    legacy_type <- c(region = 1L, industry = 2L, registration = 3L)
    if (!length(header_candidates)) {
      header <- 1L
      type <- names(legacy_type)[match(table_index, legacy_type)]
      if (is.na(type)) next
    } else {
      header <- header_candidates[[1L]]
      if (header >= nrow(table)) next
      type <- if (grepl("地区|区域", normalized_text[[header]])) "region" else if (grepl("行业", normalized_text[[header]])) "industry" else "registration"
    }
    if (row$unit_type == "private" && type == "registration") next
    current_col <- which(grepl(paste0(row$year, "年"), table[header, ]))[1L]
    growth_col <- which(grepl("增长|增速", table[header, ]))[1L]
    if (is.na(current_col) || is.na(growth_col)) next
    mapper <- switch(type, region = map_region, industry = map_industry, registration = map_registration)
    for (row_index in seq.int(header + 1L, nrow(table))) {
      label <- trimws(as.character(table[[1L]][[row_index]]))
      entity <- mapper(label)
      if (is.na(entity)) next
      for (spec in list(list(column = current_col, measure = "average_wage", unit = "Yuan"), list(column = growth_col, measure = "nominal_growth_rate", unit = "Percent"))) {
        raw <- trimws(as.character(table[[spec$column]][[row_index]]))
        value <- numeric_value(raw)
        if (is.na(value)) stop(
          "Non-numeric retained NBS wage cell: ", label, " = `", raw,
          "` in ", row$source_url[[1L]], call. = FALSE
        )
        record <- empty_record()
        record$manifest_version <- manifest_version
        record$dataset_id <- dataset_for[[row$unit_type]]
        record$release_period <- as.character(row$year)
        record$industry <- if (type == "industry") entity else "not_applicable"
        record$region <- if (type == "region") entity else "not_applicable"
        record$registration <- if (type == "registration") entity else "not_applicable"
        record$measure <- spec$measure
        record$period_basis <- "annual"
        record$unit_type <- row$unit_type
        record$unit <- spec$unit
        record$section <- type
        record$row_label <- label
        record$table_index <- as.character(table_index)
        record$row_index <- as.character(row_index)
        record$column_index <- as.character(spec$column)
        record$raw_value <- raw
        record$value <- as.character(value)
        record$observation_date <- sprintf("%d-01-01", row$year)
        record$schema_version <- classification_for(row$year, row$unit_type)
        record$basket_version <- record$schema_version
        record$classification_id <- record$schema_version
        record$seasonal_adjustment <- "not_seasonally_adjusted"
        record$release_source_url <- row$source_url
        record$classification <- record$classification_id
        record$table <- type
        records[[length(records) + 1L]] <- as.data.frame(record, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(records)) stop("No wage tables parsed from ", row$source_url[[1L]], call. = FALSE)
  result <- do.call(rbind, records)
  result
}

pieces <- lapply(seq_len(nrow(source_rows)), function(i) parse_one(source_rows[i, ]))
observations <- do.call(rbind, pieces)
if (anyDuplicated(paste(observations$dataset_id, observations$classification, observations$table, observations$region, observations$industry, observations$registration, observations$measure, observations$observation_date, sep = "\r"))) {
  stop("Wage history contains duplicated exact tuple/date observations.", call. = FALSE)
}

archive_path <- file.path(root, "inst", "extdata", "nbs-wages-history-observations.csv.gz")
archive_connection <- gzfile(archive_path, "wt")
write.csv(observations, archive_connection, row.names = FALSE, na = "")
close(archive_connection)
archive_sha <- digest(file = archive_path, algo = "sha256", serialize = FALSE)

release_index <- do.call(rbind, lapply(seq_len(nrow(source_rows)), function(i) {
  row <- source_rows[i, ]
  result <- observations[observations$dataset_id == dataset_for[[row$unit_type]] & observations$release_period == as.character(row$year), , drop = FALSE]
  data.frame(
    manifest_version = manifest_version, dataset_id = dataset_for[[row$unit_type]],
    period = as.character(row$year), reference_period_start = sprintf("%d-01-01", row$year),
    reference_period_end = sprintf("%d-12-31", row$year), release_date = release_date(row$raw_path, row$year),
    source_url = row$source_url, basket_version = classification_for(row$year, row$unit_type),
    index_base = "", expected_press_cells = nrow(result), schema_version = classification_for(row$year, row$unit_type),
    storage = "snapshot", snapshot_sha256 = archive_sha, source_capture_sha256 = row$source_capture_sha256,
    stringsAsFactors = FALSE
  )
}))
write.csv(release_index, file.path(root, "inst", "extdata", "nbs-wages-history-release-index.csv"), row.names = FALSE, na = "")

classification_labels <- c(
  non_private_employees_gbt4754_2002 = "Non-private in-post employees, GB/T 4754-2002",
  private_employees_gbt4754_2002 = "Private-unit employees, GB/T 4754-2002",
  gbt4754_2011 = "GB/T 4754-2011", gbt4754_2017 = "GB/T 4754-2017"
)
measure_labels <- c(average_wage = "Average annual wage", nominal_growth_rate = "Nominal growth rate")
region_label <- function(value) {
  result <- maps$region[value][[1L]]
  if (is.na(result) || !nzchar(result)) result <- tools::toTitleCase(gsub("_", " ", value))
  result
}
dimension_rows <- list()
for (dataset_id in unique(observations$dataset_id)) {
  members <- observations[observations$dataset_id == dataset_id, , drop = FALSE]
  for (dimension in c("classification", "table", "region", "industry", "registration", "measure")) {
    for (value in sort(unique(members[[dimension]]))) {
      dates <- members$observation_date[members[[dimension]] == value]
      label <- switch(dimension,
        classification = classification_labels[[value]], table = tools::toTitleCase(value),
        region = if (value == "not_applicable") "Not applicable" else region_label(value),
        industry = if (value == "not_applicable") "Not applicable" else maps$industry[value][[1L]],
        registration = if (value == "not_applicable") "Not applicable" else maps$registration[value][[1L]],
        measure = measure_labels[[value]]
      )
      dimension_rows[[length(dimension_rows) + 1L]] <- data.frame(
        manifest_version = manifest_version, dataset_id = dataset_id, dimension = dimension,
        value = value, label_en = label, label_zh = label, route_value = value,
        sort_order = length(dimension_rows) + 1L, valid_from = min(dates), valid_to = max(dates),
        unit_en = if (dimension == "measure" && value == "average_wage") "Yuan" else if (dimension == "measure") "Percent" else "",
        unit_zh = if (dimension == "measure" && value == "average_wage") "元" else if (dimension == "measure") "%" else "",
        semantics = if (dimension == "measure") "single_period" else "",
        stringsAsFactors = FALSE
      )
    }
  }
}
dimensions <- do.call(rbind, dimension_rows)
write.csv(dimensions, file.path(root, "inst", "extdata", "nbs-wages-history-dataset-dimensions.csv"), row.names = FALSE, na = "")

source_lookup <- split(source_rows, source_rows$unit_type)
manifest <- do.call(rbind, lapply(names(dataset_for), function(unit_type) {
  member <- observations[observations$dataset_id == dataset_for[[unit_type]], , drop = FALSE]
  expected_cells <- max(release_index$expected_press_cells[
    release_index$dataset_id == dataset_for[[unit_type]]
  ])
  data.frame(
    manifest_version = manifest_version, dataset_id = dataset_for[[unit_type]], release_id = "cn_nbs_wages_history",
    family = "wages", title_en = paste(
      "Urban", gsub("_", "-", unit_type),
      "wage source tables (classification-era archive)"
    ),
    title_zh = if (unit_type == "private") "城镇私营单位就业人员平均工资历史" else "城镇非私营单位就业人员平均工资历史",
    description_en = paste("Official annual average wage and nominal growth by source table and explicit classification era for urban", gsub("_", "-", unit_type), "units."),
    description_zh = "按原始发布表及明确行业分类时期保留的官方年度平均工资和名义增长率。",
    frequency = "A", route = "press_html_table", default_start = "2010-01-01", available_to = "2021-12-31",
    coverage_en = "2010-2021 retained Chinese NBS release tables; explicit GB/T 4754-2002, 2011 and 2017 eras; prior-year column excluded to prevent duplicate-vintage observations.",
    coverage_zh = "2010—2021年国家统计局中文发布表；GB/T 4754-2002、2011和2017时期明确分开；上年列不重复保留。",
    required_dimensions = "classification|table|region|industry|registration|measure", panel_allowed = "TRUE",
    expected_rows = length(unique(member$row_label)), expected_cells = expected_cells,
    aggregation_basis = "single_period", seasonal_adjustment = "not_applicable", derived_method = "none",
    source_vintage = "first_release_snapshot", series_note_en = paste(
      "Source-table archive, not one continuous series.",
      "Classification and coverage changes are separate selector values and",
      "are never inferred across eras; use the longitudinal wage dataset for charts."
    ),
    series_note_zh = "分类和覆盖变化作为独立选择项，绝不跨时期推断拼接。", unit_en = "Yuan or percent", unit_zh = "元或百分比",
    source_url = source_lookup[[unit_type]]$source_url[[1L]], source_note = "Retained official NBS Chinese annual wage releases with source SHA-256.",
    period_policy = "all_periods", stringsAsFactors = FALSE
  )
}))
write.csv(manifest, file.path(root, "inst", "extdata", "nbs-wages-history-dataset-manifest.csv"), row.names = FALSE, na = "")

cat(sprintf("wage observations: %d; compressed archive: %.1f KiB; SHA-256: %s\n", nrow(observations), file.info(archive_path)$size / 1024, archive_sha))
print(with(release_index, table(dataset_id)))
