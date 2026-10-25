#!/usr/bin/env Rscript

# Offline generator for the retained NBS Yearbook archive.  It consumes the
# byte-exact snapshots in inst/extdata, verifies each source SHA before decode,
# and never contacts NBS.  The first invocation below freezes the label map;
# every ordinary build then fails closed on a new Chinese label.

args <- commandArgs(trailingOnly = TRUE)
bootstrap <- identical(args, "--bootstrap-label-map")
if (length(args) && !bootstrap) {
  stop("Usage: build_nbs_yearbook_archive.R [--bootstrap-label-map]", call. = FALSE)
}

source(file.path("R", "conditions.R"), local = TRUE)
source(file.path("R", "utils.R"), local = TRUE)
source(file.path("R", "datasets.R"), local = TRUE)
source(file.path("R", "http.R"), local = TRUE)
source(file.path("R", "yearbook.R"), local = TRUE)

# The retained 2011 tables are Chinese-language HTML.  Keep this translation
# layer with the offline builder (rather than the runtime adapter) so that the
# frozen map remains a reviewable, source-derived record.  Units come from the
# table unit row or the parent metric heading where a rowspan omits it on a
# child row.
.nbs_yearbook_metadata <- function(map) {
  compact <- function(x) gsub("[[:space:]]+", "", x)
  clean_label <- function(label, measure) {
    label <- sub("^/\\s*", "", label)
    label <- trimws(label)
    if (identical(measure, "value")) label else paste(label, measure, sep = " / ")
  }
  metric14 <- c(
    "企业/单位数/(个)" = "Enterprises", "工业/总产值/(当年价格)" = "Gross industrial output (current prices)",
    "资产总计" = "Total assets", "主营业务/收入" = "Main business revenue",
    "利润总额" = "Total profit", "全部从业/人员年平均/人数(万人)" = "Annual average employees"
  )
  group14 <- c("Total", "Light industry", "Heavy industry", "Large enterprises",
    "Medium enterprises", "Small enterprises", "Domestic-funded enterprises",
    "State-owned enterprises", "Collective enterprises", "Shareholding cooperative enterprises",
    "Joint-venture enterprises", "State-owned joint-venture enterprises",
    "Collective joint-venture enterprises", "State and collective joint-venture enterprises",
    "Other joint-venture enterprises", "Limited liability companies",
    "Solely state-owned companies", "Other limited liability companies",
    "Joint-stock limited companies", "Private enterprises", "Private sole proprietorships",
    "Private partnerships", "Private limited liability companies",
    "Private joint-stock limited companies", "Other enterprises",
    "Hong Kong, Macao and Taiwan-invested enterprises",
    "Equity joint ventures (Hong Kong, Macao and Taiwan)",
    "Contractual joint ventures (Hong Kong, Macao and Taiwan)",
    "Solely Hong Kong, Macao and Taiwan-owned enterprises",
    "Hong Kong, Macao and Taiwan-invested shareholding enterprises",
    "Foreign-invested enterprises", "Sino-foreign equity joint ventures",
    "Sino-foreign contractual joint ventures", "Wholly foreign-owned enterprises",
    "Foreign-invested joint-stock limited companies")
  wage_industry <- c("National total", "Agriculture, forestry, animal husbandry and fishery",
    "Mining", "Manufacturing", "Production and supply of electricity, gas and water",
    "Construction", "Transport, storage and post", "Information transmission, computer services and software",
    "Wholesale and retail", "Accommodation and catering", "Finance", "Real estate",
    "Leasing and business services", "Scientific research, technical services and geological prospecting",
    "Water conservancy, environment and public-facilities management",
    "Resident services and other services", "Education", "Health, social security and social welfare",
    "Culture, sports and entertainment", "Public administration and social organisations")
  wage_industry_zh <- c("全国总计", "农、林、牧、渔业", "采矿业", "制造业", "电力、燃气及水的生产和供应业",
    "建筑业", "交通运输、仓储和邮政业", "信息传输、计算机服务和软件业", "批发和零售业", "住宿和餐饮业", "金融业", "房地产业",
    "租赁和商务服务业", "科学研究、技术服务和地质勘查业", "水利、环境和公共设施管理业", "居民服务和其他服务业", "教育", "卫生、社会保障和社会福利业",
    "文化、体育和娱乐业", "公共管理和社会组织")
  wage_owner <- c("Total", "State-owned units", "Urban collective units", "Other units")
  wage_owner_zh <- c("合计", "国有单位", "城镇集体单位", "其他单位")
  group14_zh <- c("总计", "轻工业", "重工业", "大型企业", "中型企业", "小型企业", "内资企业", "国有企业", "集体企业", "股份合作企业", "联营企业", "国有联营企业", "集体联营企业", "国有与集体联营企业", "其他联营企业", "有限责任公司", "国有独资公司", "其他有限责任公司", "股份有限公司", "私营企业", "私营独资企业", "私营合伙企业", "私营有限责任公司", "私营股份有限公司", "其他企业", "港、澳、台商投资企业", "合资经营企业（港或澳、台资）", "合作经营企业（港或澳、台资）", "港、澳、台商独资经营企业", "港、澳、台商投资股份", "外商投资企业", "中外合资经营企业", "中外合作经营企业", "外资企业", "外商投资股份有限公司")
  property_en <- c(
    "Number of property development enterprises", "Domestic-funded enterprises", "State-owned enterprises", "Collective enterprises", "Hong Kong, Macao and Taiwan-invested enterprises", "Foreign-invested enterprises",
    "Average employees", "Domestic-funded enterprises — employees", "State-owned enterprises — employees", "Collective enterprises — employees", "Hong Kong, Macao and Taiwan-invested enterprises — employees", "Foreign-invested enterprises — employees",
    "Land purchased during the year", "Investment completed during the year", "Residential investment completed", "Affordable-housing investment completed",
    "Subtotal of funding sources", "Domestic loans", "Foreign capital utilised", "Self-raised funds",
    "Building floor space under construction", "Building floor space completed", "Newly started floor space", "Residential floor space under construction", "Affordable-housing floor space under construction",
    "Commercial housing sales floor space", "Residential commercial-housing sales floor space", "Affordable-housing sales floor space",
    "Average sales price of commercial housing", "Average sales price of residential commercial housing", "Average sales price of affordable housing",
    "Paid-in capital", "State capital", "Asset-liability ratio", "Main business revenue", "Land-transfer revenue")
  property_zh <- c(
    "房地产开发企业个数（个）", "内资房地产开发企业个数（个）", "国有房地产开发企业个数（个）", "集体房地产开发企业个数（个）", "港、澳、台投资房地产开发企业个数（个）", "外商投资房地产开发企业个数（个）",
    "房地产开发企业平均从业人数（万人）", "内资房地产开发企业平均从业人数（万人）", "国有房地产开发企业平均从业人数（万人）", "集体房地产开发企业平均从业人数（万人）", "港、澳、台投资房地产开发企业平均从业人数（万人）", "外商投资房地产开发企业平均从业人数（万人）",
    "本年土地购置面积（万平方米）", "本年完成投资额（亿元）", "住宅本年完成投资额（亿元）", "经济适用房本年完成投资额（亿元）",
    "资金来源小计（亿元）", "国内贷款（亿元）", "利用外资（亿元）", "自筹资金（亿元）",
    "房屋建筑施工面积（万平方米）", "房屋建筑竣工面积（万平方米）", "本年房屋新开工面积（万平方米）", "住宅施工面积（万平方米）", "经济适用房施工面积（万平方米）",
    "商品房销售面积（万平方米）", "住宅商品房销售面积（万平方米）", "经济适用房销售面积（万平方米）",
    "商品房平均销售价格（元/平方米）", "住宅商品房平均销售价格（元/平方米）", "经济适用房平均销售价格（元/平方米）",
    "实收资本合计（亿元）", "国家资本金（亿元）", "资产负债率（%）", "主营业务收入（亿元）", "土地转让收入（亿元）")
  property_unit_en <- c(rep("Enterprises", 6), rep("10,000 persons", 6), "10,000 square metres", rep("100 million yuan", 7), rep("10,000 square metres", 8), rep("Yuan per square metre", 3), "100 million yuan", "100 million yuan", "Percent", "100 million yuan", "100 million yuan")
  property_unit_zh <- c(rep("个", 6), rep("万人", 6), "万平方米", rep("亿元", 7), rep("万平方米", 8), rep("元/平方米", 3), "亿元", "亿元", "百分比", "亿元", "亿元")
  household_en <- c(
    "Urban households: employed persons per household", "Rural households: full- and half-labourers per household", "Urban households: dependants per employed person", "Rural households: dependants per labourer", "Registered urban unemployed persons", "Registered urban unemployment rate",
    "Urban residents: per-capita disposable income", "Rural residents: per-capita net income", "Urban residents: per-capita consumption expenditure", "Rural residents: per-capita living consumption expenditure", "Per-capita savings-deposit balance", "Engel coefficient: urban households", "Engel coefficient: rural households",
    "Urban per-capita residential floor space", "Rural per-capita housing floor space", "Urban public buses per 10,000 population", "Urban per-capita road area", "Urban households: cars per 100 households", "Rural households: motorcycles per 100 households",
    "Telephone penetration rate (including mobile phones)", "Mobile-phone penetration rate", "Urban water-supply coverage rate", "Urban gas-supply coverage rate", "Per-capita urban park green space", "Public toilets per 10,000 population", "Per-capita domestic tourism expenditure", "Per-capita domestic tourism expenditure: urban", "Per-capita domestic tourism expenditure: rural",
    "Radio-programme population coverage", "Television-programme population coverage", "Colour televisions per 100 urban households", "Colour televisions per 100 rural households", "Computers per 100 urban households", "Computers per 100 rural households", "Education, culture and recreation expenditure share: urban households", "Education, culture and recreation expenditure share: rural households",
    "Senior-secondary-school progression rate", "Junior-secondary-school progression rate", "Primary-school progression rate", "Hospital and health-centre beds per 10,000 population", "Practising (assistant) physicians per 10,000 population", "Health-care expenditure share: urban households", "Health-care expenditure share: rural households",
    "Participants in basic old-age insurance for urban enterprise employees", "Employees covered by basic old-age insurance for urban enterprise employees", "Retirees covered by basic old-age insurance for urban enterprise employees", "Employees and retirees covered by basic urban medical insurance", "Unemployment-insurance participants", "Work-injury-insurance participants", "Maternity-insurance participants", "Social-insurance fund revenue")
  household_unit_en <- c("Persons", "Persons", "Persons", "Persons", "10,000 persons", "Percent", rep("Yuan", 5), "Percent", "Percent", "Square metres", "Square metres", "Standard vehicles", "Square metres", "Vehicles", "Vehicles", "Handsets per 100 persons", "Handsets per 100 persons", "Percent", "Percent", "Square metres", "Seats", "Yuan", "Yuan", "Yuan", "Percent", "Percent", "Sets per 100 households", "Sets per 100 households", "Sets per 100 households", "Sets per 100 households", "Percent", "Percent", "Percent", "Percent", "Percent", "Beds", "Persons", "Percent", "Percent", rep("10,000 persons", 7), "100 million yuan")
  household_unit_zh <- c("人", "人", "人", "人", "万人", "百分比", rep("元", 5), "百分比", "百分比", "平方米", "平方米", "标台", "平方米", "辆", "辆", "部/百人", "部/百人", "百分比", "百分比", "平方米", "座", "元", "元", "元", "百分比", "百分比", "部/百户", "部/百户", "部/百户", "部/百户", "百分比", "百分比", "百分比", "百分比", "百分比", "张", "人", "百分比", "百分比", rep("万人", 7), "亿元")
  region_en <- c("National", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang")
  region_zh <- c("全国", "北京", "天津", "河北", "山西", "内蒙古", "辽宁", "吉林", "黑龙江", "上海", "江苏", "浙江", "安徽", "福建", "江西", "山东", "河南", "湖北", "湖南", "广东", "广西", "海南", "重庆", "四川", "贵州", "云南", "西藏", "陕西", "甘肃", "青海", "宁夏", "新疆")
  map$dimension_label_zh <- mapply(clean_label, map$source_label_zh, map$source_measure_zh, USE.NAMES = FALSE)
  map$dimension_label_en <- NA_character_
  map$unit_en <- NA_character_; map$unit_zh <- NA_character_
  for (i in seq_len(nrow(map))) {
    table <- map$source_table[[i]]; row <- as.integer(map$source_row_index[[i]])
    if (table == "3-1") {
      measure <- compact(map$source_measure_zh[[i]])
      lookup <- c("总人口(年末)" = "Year-end total population", "按城乡分/城镇/人口数" = "Urban population", "按城乡分/城镇/比重(%)" = "Urban population share", "按城乡分/乡村/人口数" = "Rural population", "按城乡分/乡村/比重(%)" = "Rural population share", "按性别分/女/人口数" = "Female population", "按性别分/女/比重(%)" = "Female population share", "按性别分/男/人口数" = "Male population", "按性别分/男/比重(%)" = "Male population share")
      map$dimension_label_en[[i]] <- paste0("2010 population — ", lookup[[measure]])
      if (grepl("%", measure, fixed = TRUE)) { map$unit_en[[i]] <- "Percent"; map$unit_zh[[i]] <- "百分比" } else { map$unit_en[[i]] <- "10,000 persons"; map$unit_zh[[i]] <- "万人" }
    } else if (table == "4-15") {
      map$dimension_label_en[[i]] <- sprintf("2010 average wage of urban-unit employees — %s — %s", wage_industry[ceiling(row / 4)], wage_owner[(row - 1L) %% 4L + 1L])
      map$dimension_label_zh[[i]] <- sprintf("2010年城镇单位就业人员平均工资—%s—%s", wage_industry_zh[ceiling(row / 4)], wage_owner_zh[(row - 1L) %% 4L + 1L])
      map$unit_en[[i]] <- "Yuan"; map$unit_zh[[i]] <- "元"
    } else if (table == "5-27") {
      position <- row / 4L
      map$dimension_label_en[[i]] <- paste0("2010 property development enterprises — ", property_en[[position]])
      map$dimension_label_zh[[i]] <- property_zh[[position]]
      map$unit_en[[i]] <- property_unit_en[[position]]; map$unit_zh[[i]] <- property_unit_zh[[position]]
    } else if (table == "10-1") {
      position <- match(row, c(4L, 8L, 12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L, 44L, 48L, 52L, 54L, 58L, 62L, 66L, 69L, 73L, 77L, 81L, 85L, 89L, 93L, 97L, 100L, 103L, 106L, 110L, 114L, 118L, 122L, 125L, 128L, 132L, 136L, 140L, 144L, 148L, 152L, 156L, 160L, 164L, 168L, 172L, 176L, 179L, 182L, 185L, 188L, 192L))
      if (is.na(position) || position > length(household_en)) stop(sprintf("Unmapped household metadata row %s (position %s of %s).", row, position, length(household_en)), call. = FALSE)
      map$dimension_label_en[[i]] <- paste0("2010 household and public-service indicator — ", household_en[[position]])
      map$unit_en[[i]] <- household_unit_en[[position]]; map$unit_zh[[i]] <- household_unit_zh[[position]]
    } else if (table == "14-1") {
      group <- group14[ceiling(row / 6)]
      metric <- metric14[[compact(map$source_measure_zh[[i]])]]
      map$dimension_label_en[[i]] <- sprintf("2010 industrial enterprises above designated size — %s — %s", group, metric)
      map$dimension_label_zh[[i]] <- sprintf("2010年规模以上工业企业—%s—%s", group14_zh[ceiling(row / 6)], compact(map$source_measure_zh[[i]]))
      if (grepl("单位数", compact(map$source_measure_zh[[i]]), fixed = TRUE)) { map$unit_en[[i]] <- "Enterprises"; map$unit_zh[[i]] <- "个" } else if (grepl("人数", compact(map$source_measure_zh[[i]]), fixed = TRUE)) { map$unit_en[[i]] <- "10,000 persons"; map$unit_zh[[i]] <- "万人" } else { map$unit_en[[i]] <- "100 million yuan"; map$unit_zh[[i]] <- "亿元" }
    } else if (table == "17-22") {
      position <- ((row - 3L) %/% 4L) + 1L
      growth <- grepl("增", map$source_measure_zh[[i]], fixed = TRUE)
      map$dimension_label_en[[i]] <- sprintf("2010 retail sales — %s — %s", region_en[[position]], if (growth) "growth" else "total retail sales")
      map$dimension_label_zh[[i]] <- sprintf("2010年%s社会消费品零售总额%s", region_zh[[position]], if (growth) "增长率（%）" else "（亿元）")
      map$unit_en[[i]] <- if (growth) "Percent" else "100 million yuan"; map$unit_zh[[i]] <- if (growth) "百分比" else "亿元"
    }
  }
  if (anyNA(map$dimension_label_en) || anyNA(map$unit_en) || anyNA(map$unit_zh) || any(grepl("Official Yearbook|Official published unit", unlist(map[c("dimension_label_en", "unit_en")]), fixed = TRUE))) stop("Yearbook metadata enrichment left a placeholder or missing label.", call. = FALSE)
  map
}

snapshot_index <- .nbs_load_yearbook_snapshot_index()
targets <- nbs_yearbook_targets(2011L)
routes <- merge(
  snapshot_index,
  targets,
  by = c("source_yearbook", "source_table", "family", "schema_era"),
  all.x = TRUE, sort = FALSE
)
if (nrow(routes) != nrow(snapshot_index) || anyNA(routes$expected_columns)) {
  stop("Every frozen Yearbook snapshot needs one geometry target.", call. = FALSE)
}

parsed <- lapply(seq_len(nrow(routes)), function(index) {
  route <- routes[index, c(
    "source_yearbook", "family", "source_table", "source_url", "schema_era",
    "expected_columns", "expected_min_rows", "header_rows"
  ), drop = FALSE]
  raw <- .nbs_yearbook_snapshot_raw(routes[index, , drop = FALSE])
  html <- .nbs_yearbook_decode_html(raw, routes$encoding[[index]])
  nbs_yearbook_parse_table(
    html, route,
    retrieved_at = as.POSIXct(routes$retrieved_at[[index]], tz = "UTC"),
    raw_sha256 = routes$raw_sha256[[index]]
  )
})
parsed <- do.call(rbind, parsed)
parsed$source_row_index <- ave(
  seq_len(nrow(parsed)), parsed$source_table, FUN = seq_along
)
parsed <- parsed[parsed$reference_year == 2010L, , drop = FALSE]

label_map_path <- file.path("inst", "extdata", "nbs-yearbook-label-map.csv")
if (bootstrap) {
  label_map <- .nbs_yearbook_default_mapping(parsed)
  label_map <- .nbs_yearbook_metadata(label_map)
  label_map <- label_map[order(
    label_map$source_table, label_map$source_label_zh,
    label_map$source_measure_zh
  ), , drop = FALSE]
  utils::write.csv(label_map, label_map_path, row.names = FALSE,
    fileEncoding = "UTF-8")
  cat("Wrote frozen Yearbook Chinese-label map: ", label_map_path, "\n", sep = "")
} else {
  label_map <- .nbs_load_yearbook_label_map()
}

archive <- lapply(seq_len(nrow(snapshot_index)), function(index) {
  rows <- parsed[
    parsed$source_table == snapshot_index$source_table[[index]], , drop = FALSE
  ]
  .nbs_yearbook_adapt_rows(rows, snapshot_index$dataset_id[[index]], label_map)
})
archive <- do.call(rbind, archive)
archive <- archive[order(archive$dataset_id, archive$series), , drop = FALSE]
if (anyDuplicated(paste(archive$dataset_id, archive$release_period, archive$series, sep = "\r"))) {
  stop("The Yearbook adapter produced duplicate exact tuples.", call. = FALSE)
}

archive_path <- file.path("inst", "extdata", "nbs-yearbook-observations.csv.gz")
connection <- gzfile(archive_path, open = "wt", encoding = "UTF-8")
utils::write.csv(archive, connection, row.names = FALSE, na = "")
close(connection)
archive_sha <- digest::digest(file = archive_path, algo = "sha256", serialize = FALSE)

dimensions <- unique(archive[c(
  "dataset_id", "series", "dimension_label_en", "dimension_label_zh",
  "unit_en", "unit_zh"
)])
dimensions <- dimensions[order(dimensions$dataset_id, dimensions$series), , drop = FALSE]
dimensions <- data.frame(
  manifest_version = "2026-08-11.1",
  dataset_id = dimensions$dataset_id,
  dimension = "series",
  value = dimensions$series,
  label_en = dimensions$dimension_label_en,
  label_zh = dimensions$dimension_label_zh,
  route_value = dimensions$series,
  sort_order = ave(dimensions$series, dimensions$dataset_id,
    FUN = function(value) seq_along(value)),
  valid_from = "2010-01-01",
  valid_to = "2010-12-31",
  unit_en = dimensions$unit_en,
  unit_zh = dimensions$unit_zh,
  semantics = "retained_official_yearbook_annual_snapshot",
  stringsAsFactors = FALSE
)
utils::write.csv(dimensions,
  file.path("inst", "extdata", "nbs-yearbook-dataset-dimensions.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")

release_rows <- lapply(seq_len(nrow(snapshot_index)), function(index) {
  snapshot <- snapshot_index[index, , drop = FALSE]
  cells <- sum(archive$dataset_id == snapshot$dataset_id)
  data.frame(
    manifest_version = "2026-08-11.1",
    dataset_id = snapshot$dataset_id,
    period = "2010",
    reference_period_start = "2010-01-01",
    reference_period_end = "2010-12-31",
    # NBS retained pages have no machine-readable publication day.  This is
    # explicitly the freeze timestamp, while source_yearbook carries vintage.
    release_date = substr(snapshot$retrieved_at, 1L, 10L),
    source_url = snapshot$source_url,
    schema_era = snapshot$schema_era,
    expected_yearbook_cells = cells,
    storage = "yearbook_snapshot",
    snapshot_sha256 = archive_sha,
    source_capture_sha256 = snapshot$raw_sha256,
    stringsAsFactors = FALSE
  )
})
release_index <- do.call(rbind, release_rows)
utils::write.csv(release_index,
  file.path("inst", "extdata", "nbs-yearbook-release-index.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("Wrote %d Yearbook observations across %d annual releases.\n",
  nrow(archive), nrow(release_index)))
