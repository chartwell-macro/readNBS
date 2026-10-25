#!/usr/bin/env Rscript

# Build the source-faithful 2012--24 migrant-worker monitoring archive.
#
# This deliberately archives *table cells*, not a reconstructed contemporary
# migrant-worker panel.  A report often repeats older observations and its
# headings, classifications and table coverage change materially over time.
# Keeping the release year in every cell avoids presenting a later revision as
# an earlier first release or pretending that the 2025 selectors existed in
# older reports.  The script is offline: it only reads the retained official
# NBS HTML captures in inst/extdata/nbs-migrant-history/.

suppressPackageStartupMessages({
  library(digest)
  library(rvest)
})

root <- normalizePath(getwd(), mustWork = TRUE)
out <- file.path(root, "inst", "extdata")
raw_dir <- file.path(out, "nbs-migrant-history")
version <- "2026-08-13.1"
dataset_id <- "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"

sources <- data.frame(
  release_period = as.character(2012:2024),
  release_date = c(
    "2013-05-27", "2014-05-12", "2015-04-29", "2016-04-28",
    "2017-04-28", "2018-04-27", "2019-04-29", "2020-04-30",
    "2021-04-30", "2022-04-29", "2023-04-28", "2024-04-30",
    "2025-04-30"
  ),
  source_url = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898305.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898505.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898768.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899104.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899495.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899920.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900299.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900710.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901074.html",
    "https://www.stats.gov.cn/xxgk/sjfb/zxfb2020/202204/t20220429_1830139.html",
    "https://www.stats.gov.cn/sj/zxfb/202304/t20230427_1939124.html",
    "https://www.stats.gov.cn/sj/zxfb/202404/t20240430_1948783.html",
    "https://www.stats.gov.cn/xxgk/sjfb/zxfb2020/202504/t20250430_1959523.html"
  ),
  stringsAsFactors = FALSE
)
sources$raw_path <- file.path(raw_dir, paste0(sources$release_period, ".html"))
if (any(!file.exists(sources$raw_path))) {
  stop("Missing retained official NBS migrant-worker capture(s).", call. = FALSE)
}

trim <- function(x) trimws(gsub("[[:space:]　]+", " ", enc2utf8(as.character(x))))
is_number <- function(x) {
  x <- gsub("[[:space:],，]", "", trim(x))
  grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$", x)
}
as_number <- function(x) as.numeric(gsub("[[:space:],，]", "", trim(x)))
table_signature <- function(x) paste(as.matrix(x), collapse = "\r")

# NBS uses the preceding non-empty, non-numeric cells as a compact native
# header path.  We retain it verbatim rather than translating or normalising a
# changing classification into the 2025 schema.
header_path <- function(table, row, column) {
  values <- character()
  if (row > 1L) values <- c(values, table[seq_len(row - 1L), column])
  if (column > 1L) values <- c(values, table[row, seq_len(column - 1L)])
  values <- trim(values)
  values <- values[nzchar(values) & !is_number(values)]
  paste(unique(values), collapse = " | ")
}

has <- function(pattern, x) grepl(pattern, x, perl = TRUE)

# Finite, source-specific translation dictionaries.  These are intentionally
# not a general transliterator: a new native token must be reviewed and mapped,
# otherwise the offline build fails closed.  `label_zh` below retains the
# assembled native title/row/header path without translating its content.
native_key <- function(x) {
  x <- trim(x)
  x <- gsub("\u00a0|[[:space:]　]+", "", x, perl = TRUE)
  x <- chartr("１２", "12", x)
  gsub("．", ".", x, fixed = TRUE)
}

native_term_en <- c(
  "（1）住户中外出农民工" = "Migrant workers working away from home, excluding whole-family migrants",
  "（2）举家外出农民工" = "Whole-family migrant workers working away from home",
  "1.外出农民工" = "Migrant workers working away from their home township",
  "2.本地农民工" = "Local migrant workers",
  "农民工" = "Migrant workers", "全部农民工" = "All migrant workers",
  "全部" = "All", "农民工合计" = "Migrant workers, total",
  "农民工总量" = "Migrant workers, total", "农民工规模" = "Migrant-worker population",
  "规模" = "Population", "人数" = "Persons", "合计" = "Total",
  "外出" = "Working away from home", "外出农民工" = "Migrant workers working away from home",
  "外出农民工人数" = "Migrant workers working away from home, persons",
  "外出农民工总量" = "Migrant workers working away from home, total",
  "外出农民工总量（万人）" = "Migrant workers working away from home, total (10,000 persons)",
  "外出农民工构成" = "Composition of migrant workers working away from home",
  "外出农民工构成（%）" = "Composition of migrant workers working away from home (percent)",
  "其中：外出农民工" = "Of which: migrant workers working away from home",
  "本地农民工" = "Local migrant workers", "新生代农民工" = "New-generation migrant workers",
  "老一代农民工" = "Older-generation migrant workers", "非农民工" = "Non-migrant workers",
  "东部" = "Eastern region", "东部地区" = "Eastern region", "在东部地区" = "In the eastern region",
  "中部" = "Central region", "中部地区" = "Central region", "在中部地区" = "In the central region",
  "西部" = "Western region", "西部地区" = "Western region", "在西部地区" = "In the western region",
  "东北地区" = "Northeastern region", "在东北地区" = "In the northeastern region",
  "在其他地区" = "In other regions", "全国" = "National",
  "直辖市" = "Municipalities", "省会城市" = "Provincial capitals",
  "直辖市和省会城市" = "Municipalities and provincial capitals",
  "地级市" = "Prefecture-level cities",
  "地级市（包括副省级）" = "Prefecture-level cities (including sub-provincial cities)",
  "小城镇" = "Small towns", "县外省内" = "Outside county, within province",
  "乡外县内" = "Outside township, within county", "省外" = "Outside province",
  "跨省流动" = "Interprovincial movement", "其中：跨省流动" = "Of which: interprovincial movement",
  "省内流动" = "Within-province movement", "省内乡外流动" = "Outside home township, within province",
  "第一产业" = "Primary industry", "第二产业" = "Secondary industry", "第三产业" = "Tertiary industry",
  "制造业" = "Manufacturing", "其中：制造业" = "Of which: manufacturing",
  "建筑业" = "Construction", "批发和零售业" = "Wholesale and retail trade",
  "批发零售业" = "Wholesale and retail trade", "其中：批发和零售业" = "Of which: wholesale and retail trade",
  "交通运输、仓储和邮政业" = "Transport, storage and postal services",
  "交通运输仓储和邮政业" = "Transport, storage and postal services",
  "交通运输仓储邮政业" = "Transport, storage and postal services",
  "住宿和餐饮业" = "Accommodation and catering", "住宿餐饮业" = "Accommodation and catering",
  "居民服务、修理和其他服务业" = "Resident services, repairs and other services",
  "居民服务修理和其他服务业" = "Resident services, repairs and other services",
  "居民服务和其他服务业" = "Resident and other services",
  "公共管理、社会保障和社会组织" = "Public administration, social security and social organizations",
  "其他行业" = "Other industries", "其他" = "Other",
  "未上过学" = "No schooling", "不识字或识字很少" = "Illiterate or very limited literacy",
  "小学" = "Primary school", "初中" = "Junior secondary school", "中专" = "Specialized secondary school",
  "高中" = "Senior secondary school", "大专及以上" = "College diploma or above",
  "接受技能培训" = "Received skills training", "接受过技能培训" = "Received skills training",
  "接受农业技能培训" = "Received agricultural skills training",
  "接受农业技术培训" = "Received agricultural technical training",
  "参加过农业技术培训" = "Received agricultural technical training",
  "接受非农职业技能培训" = "Received non-agricultural vocational training",
  "参加过非农职业技能培训" = "Received non-agricultural vocational training",
  "接受农业或非农职业技能培训" = "Received agricultural or non-agricultural vocational training",
  "两项培训都没有参加过" = "Received neither type of training",
  "受雇就业" = "Wage employment", "自营就业" = "Self-employment",
  "人均月生活消费支出" = "Monthly living consumption expenditure per person",
  "其中：人均月居住支出" = "Of which: monthly housing expenditure per person",
  "生活消费支出（元/人）" = "Living consumption expenditure (yuan per person)",
  "其中：居住支出（元/人）" = "Of which: housing expenditure (yuan per person)",
  "居住支出占比（%）" = "Housing expenditure share (percent)", "金额" = "Amount",
  "单位宿舍" = "Employer dormitory", "工地工棚" = "Worksite shed",
  "生产经营场所" = "Production or business premises", "乡外从业回家居住" = "Works outside township and returns home",
  "务工地自购房" = "Owner-occupied housing at place of work", "独立租赁" = "Private rental",
  "独立租赁住房" = "Private rental housing", "与人合租" = "Shared rental",
  "与他人合租住房" = "Shared rental housing",
  "全年外出从业时间（月）" = "Months worked away from home during year",
  "平均每月工作时间（天）" = "Average working days per month",
  "平均每天工作时间（小时）" = "Average working hours per day",
  "日工作超过8小时的比重（%）" = "Share working more than 8 hours per day (percent)",
  "日工作超过8小时的农民工比重（%）" = "Share of migrant workers working more than 8 hours per day (percent)",
  "周工作超过44小时的比重（%）" = "Share working more than 44 hours per week (percent)",
  "周工作超过44小时的农民工比重（%）" = "Share of migrant workers working more than 44 hours per week (percent)",
  "一年以下劳动合同" = "Employment contract under one year",
  "一年及以上劳动合同" = "Employment contract of one year or longer",
  "无固定期限劳动合同" = "Open-ended employment contract", "没有劳动合同" = "No employment contract",
  "工伤保险" = "Work-injury insurance", "医疗保险" = "Medical insurance",
  "养老保险" = "Pension insurance", "失业保险" = "Unemployment insurance",
  "生育保险" = "Maternity insurance", "住房公积金" = "Housing provident fund",
  "对本地生活非常适应的农民工占比" = "Share of migrant workers very well adapted to local life",
  "认为是本地人的农民工占比" = "Share of migrant workers who regard themselves as local residents",
  "构成" = "Composition", "构成（%）" = "Composition (percent)",
  "增量" = "Change in level", "增减" = "Change", "增加" = "Change",
  "增长" = "Growth", "增长率" = "Growth rate", "增速" = "Growth rate",
  "比上年增加" = "Change from previous year"
)
names(native_term_en) <- native_key(names(native_term_en))

native_subject_en <- c(
  "农民工数量" = "Migrant-worker numbers",
  "农民工数量及构成" = "Migrant-worker numbers and composition",
  "农民工规模" = "Migrant-worker population",
  "按输出地分的农民工地区构成" = "Regional composition of migrant workers by origin",
  "不同地区外出农民工在省内外务工的分布" = "Within- and outside-province employment of migrant workers working away from home, by region",
  "农民工年龄构成" = "Age composition of migrant workers",
  "农民工文化程度构成" = "Educational composition of migrant workers",
  "农民工的文化程度构成" = "Educational composition of migrant workers",
  "不同年龄组农民工参加培训情况" = "Training participation of migrant workers by age group",
  "接受技能培训的农民工比重" = "Share of migrant workers receiving skills training",
  "接受过技能培训的农民工比例" = "Share of migrant workers receiving skills training",
  "接受过技能培训的农民工比重" = "Share of migrant workers receiving skills training",
  "农民工从事的主要行业分布" = "Main industry distribution of migrant workers",
  "农民工在不同地区从事的主要行业分布" = "Main industry distribution of migrant workers by region",
  "农民工从业行业分布" = "Industry distribution of migrant workers",
  "农民工从业行业分布占比情况" = "Industry distribution of migrant workers",
  "农民工就业行业分布" = "Employment industry distribution of migrant workers",
  "农民工就业行业分布情况" = "Employment industry distribution of migrant workers",
  "分地区的农民工产业分布" = "Industry distribution of migrant workers by region",
  "外出农民工在不同地区务工的月收入水平" = "Monthly income of migrant workers working away from home, by work region",
  "分行业农民工人均月收入及增幅" = "Monthly income per migrant worker and growth, by industry",
  "分行业农民工月均收入及增速" = "Average monthly migrant-worker income and growth, by industry",
  "外出农民工人均月生活消费支出" = "Monthly living consumption expenditure per migrant worker working away from home",
  "外出农民工在不同地区务工月均生活消费和居住支出" = "Monthly living consumption and housing expenditure of migrant workers working away from home, by work region",
  "外出农民工在不同城市类型务工月均生活消费和居住支出" = "Monthly living consumption and housing expenditure of migrant workers working away from home, by city type",
  "外出农民工的住宿情况" = "Accommodation of migrant workers working away from home",
  "外出农民工从业时间和强度" = "Work duration and intensity of migrant workers working away from home",
  "外出农民工参加社会保障的比例" = "Social-security participation of migrant workers working away from home",
  "不同行业农民工参加社会保障的比例" = "Social-security participation of migrant workers by industry",
  "分行业农民工被拖欠工资的比重" = "Share of migrant workers with wage arrears, by industry",
  "农民工参加“五险一金”的比例" = "Migrant-worker participation in five social insurances and the housing provident fund",
  "分地区农民工参加“五险一金”的比例" = "Migrant-worker participation in five social insurances and the housing provident fund, by region",
  "分行业农民工参加“五险一金”的比例" = "Migrant-worker participation in five social insurances and the housing provident fund, by industry",
  "农民工签订劳动合同情况" = "Employment contracts of migrant workers",
  "进城农民工对所在城市的归属感" = "Sense of belonging in the city among migrant workers living in urban areas",
  "外出农民工地区分布及构成" = "Regional distribution and composition of migrant workers working away from home",
  "外出农民工流向地区分布及构成" = "Destination distribution and composition of migrant workers working away from home",
  "农民工地区分布" = "Regional distribution of migrant workers",
  "农民工输出和输入地区分布" = "Regional distribution of migrant workers by origin and destination",
  "农民工在输出地和输入地的区域分布" = "Regional distribution of migrant workers by origin and destination",
  "输出地和输入地农民工的区域分布" = "Regional distribution of migrant workers by origin and destination",
  "按输出地分的外出农民工人数及构成" = "Numbers and composition of migrant workers working away from home, by origin",
  "按城市类型分的外出农民工人数及构成" = "Numbers and composition of migrant workers working away from home, by city type",
  "按城市和住宿类型分的外出农民工人数构成" = "Composition of migrant workers working away from home, by city and accommodation type",
  "按就业方式和行业分的农民工人数构成" = "Composition of migrant workers by employment arrangement and industry",
  "分行业的农民工人数构成" = "Composition of migrant workers by industry",
  "分地区分行业的农民工人数构成" = "Composition of migrant workers by region and industry",
  "新生代农民工规模" = "New-generation migrant-worker population"
)
names(native_subject_en) <- native_key(names(native_subject_en))

translate_native_term <- function(x) {
  key <- native_key(x)
  if (!nzchar(key)) return("")
  if (has("^单位[：:]", key)) return("")
  if (has("^20[0-9]{2}年$", key)) return(sub("年$", "", key))
  if (has("^20[0-9]{2}年农民工合计$", key)) {
    return(paste0(sub("年农民工合计$", "", key), " migrant workers, total"))
  }
  if (has("^[0-9]+-[0-9]+岁$", key)) return(sub("岁$", " years", key))
  if (has("^[0-9]+岁以上$", key)) return(sub("岁以上$", " years and over", key))
  if (has("^[0-9]+岁及以下$", key)) return(sub("岁及以下$", " years and under", key))
  city_sizes <- c(
    "100-300万人城市" = "Cities with 1-3 million residents",
    "300-500万人城市" = "Cities with 3-5 million residents",
    "50-100万人城市" = "Cities with 0.5-1 million residents",
    "500万人以上城市" = "Cities with 5 million residents or more",
    "50万人以下城市和建制镇" = "Cities with fewer than 0.5 million residents and designated towns",
    "30岁以下青年农民工" = "Young migrant workers under 30"
  )
  names(city_sizes) <- native_key(names(city_sizes))
  if (key %in% names(city_sizes)) return(unname(city_sizes[[key]]))
  if (key %in% names(native_term_en)) return(unname(native_term_en[[key]]))
  stop("Unmapped native migrant-history term: ", x, call. = FALSE)
}

english_table_title <- function(release, table_index, title) {
  key <- native_key(title)
  prefix <- paste0("表", table_index)
  if (!startsWith(key, prefix)) stop("Unmapped native migrant-history table title: ", title, call. = FALSE)
  subject <- sub(paste0("^", prefix, "[：:]?"), "", key)
  subject <- sub("^20[0-9]{2}年", "", subject)
  if (!(subject %in% names(native_subject_en))) {
    stop("Unmapped native migrant-history table subject: ", subject, call. = FALSE)
  }
  sprintf("Table %s: %s", table_index, unname(native_subject_en[[subject]]))
}

# Coordinates remain because repeated headings are common and the stable leaf
# identity is the native table cell.  The translated title, row and column path
# make the selector usable without weakening that exact identity.
english_cell_label <- function(release, table_index, row, column, title,
                               row_label, header, unit_en) {
  title_en <- english_table_title(release, table_index, title)
  header_parts <- trim(strsplit(header, " | ", fixed = TRUE)[[1L]])
  header_parts <- header_parts[nzchar(header_parts)]
  header_parts <- header_parts[
    native_key(header_parts) != native_key(title) &
      native_key(header_parts) != native_key(row_label) &
      !has("^单位[：:]", native_key(header_parts))
  ]
  header_en <- unique(vapply(header_parts, translate_native_term, character(1L)))
  semantics <- c(title_en, translate_native_term(row_label), header_en)
  semantics <- semantics[nzchar(semantics)]
  label <- sprintf(
    "%s — %s — cell r%03d c%03d (%s)",
    release, paste(semantics, collapse = " — "), row, column, unit_en
  )
  if (has("[\u3400-\u4DBF\u4E00-\u9FFF]", label)) {
    stop("CJK leaked into English migrant-history label: ", label, call. = FALSE)
  }
  label
}

cell_unit <- function(table, row, column, title, row_label, header,
                      published_unit = "") {
  context <- trim(paste(title, row_label, header, sep = " | "))
  table_text <- trim(paste(table, collapse = " | "))
  unit_heading <- paste(
    regmatches(table_text, gregexpr("单位[：:][^|]+", table_text, perl = TRUE))[[1L]],
    collapse = " | "
  )
  if (nzchar(published_unit) && !has("单位[：:]", table_text)) {
    unit_heading <- trim(paste(unit_heading, published_unit, sep = " | "))
  }
  semantic_header <- header
  semantic_header <- gsub(title, "", semantic_header, fixed = TRUE)
  semantic_header <- gsub(row_label, "", semantic_header, fixed = TRUE)
  semantic_header <- gsub("单位[：:][^|]+", "", semantic_header, perl = TRUE)
  semantic_context <- gsub("单位[：:][^|]+", "", context, perl = TRUE)
  all_row_labels <- trim(table[, 1L])
  section_row_labels <- trim(table[seq_len(row), 1L])
  in_change_section <- any(has("比上年增加", section_row_labels))
  in_composition_section <- any(has(
    "^外出农民工构成([（(][%％][）)])?$", native_key(section_row_labels)
  ))
  vertical_count_rate_sections <-
    any(has("^外出农民工总量[（(]万人[）)]$", native_key(all_row_labels))) &&
    any(has("^外出农民工构成[（(][%％][）)]$", native_key(all_row_labels)))
  growth_heading <- has(
    "增速|增长率|增幅|同比增长|(^|[| ])增长($|[ |])",
    semantic_header
  )

  # Units printed directly alongside the row/column meaning take precedence.
  if (has("百分点", semantic_context)) return(c(en = "Percentage points", zh = "百分点"))
  if (has("[（(]月[）)]", context)) return(c(en = "Months per year", zh = "月/年"))
  if (has("[（(]天[）)]", context)) return(c(en = "Days per month", zh = "天/月"))
  if (has("[（(]小时[）)]", context)) return(c(en = "Hours per day", zh = "小时/天"))
  if (has("[（(]万人[）)]", context)) return(c(en = "10,000 persons", zh = "万人"))
  if (has("[（(]元[/／]人[）)]", context)) {
    if (has("月均收入|月收入", context)) return(c(en = "Yuan per worker per month", zh = "元/人/月"))
    if (has("月均|每月|人均月|生活消费|居住支出", context)) return(c(en = "Yuan per person per month", zh = "元/人/月"))
    return(c(en = "Yuan per person", zh = "元/人"))
  }
  if (has("[（(]元[）)]", context)) return(c(en = "Yuan", zh = "元"))
  if (has("[（(][%％][）)]", context)) return(c(en = "Percent", zh = "%"))

  # A mixed headcount/rate heading uses level and change columns.  A numeric
  # increase/decrease is still a headcount; only composition/growth is a rate.
  if (has("万人[^|]*[%％]", unit_heading)) {
    if (has("构成|比重|比例|占比", paste(semantic_header, row_label)) ||
        in_composition_section || growth_heading) {
      return(c(en = "Percent", zh = "%"))
    }
    return(c(en = "10,000 persons", zh = "万人"))
  }

  percentage_change <- (has("增减|增加|提高|下降|上升|回落", semantic_header) ||
    in_change_section) &&
    (has("百分点", unit_heading) ||
      has("构成|比重|比例|第一产业|第二产业|第三产业", semantic_context) ||
      (has("第一产业", table_text) && has("第二产业", table_text) &&
        has("第三产业", table_text)))
  if (percentage_change) return(c(en = "Percentage points", zh = "百分点"))
  if (growth_heading) {
    return(c(en = "Percent", zh = "%"))
  }
  if (has("构成|比重|比例|占比", semantic_header)) return(c(en = "Percent", zh = "%"))

  if (has("元[/／]人", unit_heading)) {
    if (has("月均收入|月收入", context)) return(c(en = "Yuan per worker per month", zh = "元/人/月"))
    if (has("月均|每月|人均月|生活消费|居住支出", context)) return(c(en = "Yuan per person per month", zh = "元/人/月"))
    return(c(en = "Yuan per person", zh = "元/人"))
  }
  if (has("元[^|]*[%％]", unit_heading)) {
    if (growth_heading) return(c(en = "Percent", zh = "%"))
    if (has("月均收入|月收入", context)) return(c(en = "Yuan per worker per month", zh = "元/人/月"))
    if (has("生活消费|居住支出", context)) return(c(en = "Yuan per person per month", zh = "元/人/月"))
    return(c(en = "Yuan", zh = "元"))
  }
  if (has("[%％][^|]*百分点", unit_heading)) {
    if (has("增减|增加|提高|下降|上升|回落", semantic_header) || in_change_section) {
      return(c(en = "Percentage points", zh = "百分点"))
    }
    return(c(en = "Percent", zh = "%"))
  }
  if (has("万人", unit_heading)) return(c(en = "10,000 persons", zh = "万人"))
  if (has("[%％]", unit_heading)) return(c(en = "Percent", zh = "%"))
  if (has("元", unit_heading)) {
    if (has("月均收入|月收入", context)) return(c(en = "Yuan per worker per month", zh = "元/人/月"))
    if (has("生活消费|居住支出", context)) return(c(en = "Yuan per person per month", zh = "元/人/月"))
    return(c(en = "Yuan", zh = "元"))
  }

  if (vertical_count_rate_sections) {
    if (in_composition_section) return(c(en = "Percent", zh = "%"))
    return(c(en = "10,000 persons", zh = "万人"))
  }

  # Some releases state the unit only through row/column semantics.  These are
  # native meanings, not inferred time-series continuity.
  if (has("增量", semantic_header)) return(c(en = "10,000 persons", zh = "万人"))
  if (has("按输出地分", table_text) && has("按输入地分", table_text)) {
    return(c(en = "10,000 persons", zh = "万人"))
  }
  if (has("构成|比重|比例|占比", context)) return(c(en = "Percent", zh = "%"))
  if (growth_heading) return(c(en = "Percent", zh = "%"))
  if (has("全年外出从业时间", row_label)) return(c(en = "Months per year", zh = "月/年"))
  if (has("每月工作时间", row_label)) return(c(en = "Days per month", zh = "天/月"))
  if (has("每天工作时间", row_label)) return(c(en = "Hours per day", zh = "小时/天"))
  if (has("超过[^|]*小时", row_label)) return(c(en = "Percent", zh = "%"))
  if (has("月均收入|月收入", context)) return(c(en = "Yuan per worker per month", zh = "元/人/月"))
  if (has("生活消费|居住支出", context)) return(c(en = "Yuan per person per month", zh = "元/人/月"))
  if (has("外出农民工总量|跨省流动|省内流动|省内乡外流动|按输出地分|按输入地分", context)) {
    return(c(en = "10,000 persons", zh = "万人"))
  }
  if (has("岁", row_label)) return(c(en = "Percent", zh = "%"))
  if (!nzchar(unit_heading) && has("增速", table_text) &&
      has("制造业", table_text) && !growth_heading) {
    return(c(en = "Yuan", zh = "元"))
  }
  if (has("第一产业|第二产业|第三产业|制造业|建筑业|批发.*零售业|交通运输|住宿.*餐饮业|居民服务|其他", row_label)) {
    return(c(en = "Percent", zh = "%"))
  }

  stop(
    "No explicit native unit could be resolved for table cell ",
    row, ":", column, " (", context, ").", call. = FALSE
  )
}

empty_record <- function() {
  as.list(setNames(rep("", 52L), c(
    "manifest_version", "dataset_id", "release_period", "category", "industry",
    "city_code", "city_label", "market", "size_band", "comparison", "index_base",
    "region", "measure", "period_basis", "residence", "nominal_real", "unit_type",
    "registration", "occupation", "product_code", "specification", "unit", "section",
    "row_label", "table_index", "row_index", "column_index", "raw_value", "value",
    "observation_date", "series_id", "reported_observation_date",
    "observation_period_start", "observation_period_end", "comparison_period_start",
    "comparison_period_end", "comparison_basis", "published_value_type",
    "schema_version", "basket_version", "classification_id", "coverage_version",
    "seasonal_adjustment", "release_source_url", "dimension_label_en",
    "dimension_label_zh", "unit_en", "unit_zh", "source_table",
    "source_capture_sha256"
  )))
}

parse_release <- function(source) {
  document <- read_html(source$raw_path[[1L]])
  table_nodes <- html_elements(document, "table")
  tables <- lapply(table_nodes, html_table, fill = TRUE, convert = FALSE)
  keep <- !duplicated(vapply(tables, table_signature, character(1L)))
  tables <- tables[keep]
  table_nodes <- table_nodes[keep]
  tables <- lapply(tables, function(x) as.matrix(data.frame(x, stringsAsFactors = FALSE)))
  records <- list()
  source_hash <- digest(file = source$raw_path[[1L]], algo = "sha256", serialize = FALSE)
  schema <- paste0("migrant_monitoring_", source$release_period[[1L]], "_native_tables")
  for (table_index in seq_along(tables)) {
    table <- apply(tables[[table_index]], c(1L, 2L), trim)
    if (!nrow(table) || !ncol(table)) next
    title <- table[[1L, 1L]]
    published_unit <- ""
    # From the 2019 report onward the visual title/unit are preceding DOM
    # paragraphs rather than grid cells.  Bind the nearest native heading to
    # the deduplicated table instead of substituting a coordinate placeholder.
    if (!grepl("^表[[:space:]]*[0-9０-９]+", title)) {
      preceding <- html_elements(table_nodes[[table_index]], xpath = "preceding::p[normalize-space()]")
      preceding_text <- trim(html_text2(preceding))
      title_positions <- which(has("^表[[:space:]]*[0-9０-９]+", preceding_text))
      preceding_titles <- preceding_text[title_positions]
      if (!length(preceding_titles)) {
        stop("Missing native DOM title for table ", table_index, ".", call. = FALSE)
      }
      title <- tail(preceding_titles, 1L)
      after_title <- seq_along(preceding_text) > tail(title_positions, 1L)
      preceding_units <- preceding_text[after_title & has("单位[：:]", preceding_text)]
      if (length(preceding_units)) published_unit <- tail(preceding_units, 1L)
    }
    for (row_index in seq_len(nrow(table))) {
      row_label <- table[[row_index, 1L]]
      if (!nzchar(row_label) || is_number(row_label)) next
      for (column_index in seq_len(ncol(table))) {
        raw <- table[[row_index, column_index]]
        if (!is_number(raw)) next
        measure <- sprintf(
          "release_%s_table_%02d_row_%03d_column_%03d",
          source$release_period[[1L]], table_index, row_index, column_index
        )
        header <- header_path(table, row_index, column_index)
        years <- regmatches(header, gregexpr("20[0-9]{2}年", header, perl = TRUE))[[1L]]
        observation_year <- if (length(years)) {
          sub("年$", "", years[[length(years)]])
        } else {
          source$release_period[[1L]]
        }
        label_zh <- paste(c(title, row_label, header), collapse = " — ")
        units <- cell_unit(
          table, row_index, column_index, title, row_label, header,
          published_unit
        )
        record <- empty_record()
        record$manifest_version <- version
        record$dataset_id <- dataset_id
        record$release_period <- source$release_period[[1L]]
        record$measure <- measure
        record$period_basis <- "published_table_cell"
        record$unit <- unname(units[["en"]])
        record$section <- paste0("table_", table_index)
        record$row_label <- row_label
        record$table_index <- table_index
        record$row_index <- row_index
        record$column_index <- column_index
        record$raw_value <- raw
        record$value <- as_number(raw)
        record$observation_date <- paste0(observation_year, "-01-01")
        record$reported_observation_date <- record$observation_date
        record$observation_period_start <- record$observation_date
        record$observation_period_end <- paste0(observation_year, "-12-31")
        record$published_value_type <- "published_table_cell"
        record$schema_version <- schema
        record$classification_id <- schema
        record$coverage_version <- "native_release_table"
        record$seasonal_adjustment <- "not_applicable"
        record$release_source_url <- source$source_url[[1L]]
        record$dimension_label_en <- english_cell_label(
          source$release_period[[1L]], table_index, row_index, column_index,
          title, row_label, header, unname(units[["en"]])
        )
        record$dimension_label_zh <- label_zh
        record$unit_en <- unname(units[["en"]])
        record$unit_zh <- unname(units[["zh"]])
        record$source_table <- title
        record$source_capture_sha256 <- source_hash
        records[[length(records) + 1L]] <- as.data.frame(record, stringsAsFactors = FALSE)
      }
    }
  }
  result <- do.call(rbind, records)
  if (is.null(result) || !nrow(result) ||
      anyDuplicated(paste(result$dataset_id, result$measure, sep = "\r"))) {
    stop(
      "Migrant archive has no cells or duplicate release-cell selectors for ",
      source$release_period[[1L]], ".", call. = FALSE
    )
  }
  result
}

observations <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) {
  parse_release(sources[i, , drop = FALSE])
}))
if (any(!is.finite(observations$value)) || anyDuplicated(observations$measure)) {
  stop("Migrant archive values or selectors are invalid.", call. = FALSE)
}

archive_path <- file.path(out, "nbs-migrant-history-observations.csv.gz")
connection <- gzfile(archive_path, "wb")
utils::write.csv(observations, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)
archive_hash <- digest(file = archive_path, algo = "sha256", serialize = FALSE)

sources$manifest_version <- version
sources$dataset_id <- dataset_id
sources$reference_period_start <- paste0(sources$release_period, "-01-01")
sources$reference_period_end <- paste0(sources$release_period, "-12-31")
sources$basket_version <- paste0("migrant-monitoring-", sources$release_period)
sources$index_base <- ""
sources$expected_press_cells <- vapply(sources$release_period, function(year) {
  sum(observations$release_period == year)
}, integer(1L))
sources$schema_version <- paste0("migrant_monitoring_", sources$release_period, "_native_tables")
sources$storage <- "snapshot"
sources$snapshot_sha256 <- archive_hash
sources$source_capture_sha256 <- vapply(sources$raw_path, function(path) {
  digest(file = path, algo = "sha256", serialize = FALSE)
}, character(1L))
release_index <- sources[c(
  "manifest_version", "dataset_id", "release_period", "reference_period_start",
  "reference_period_end", "release_date", "source_url", "basket_version", "index_base",
  "expected_press_cells", "schema_version", "storage", "snapshot_sha256",
  "source_capture_sha256"
)]
names(release_index)[names(release_index) == "release_period"] <- "period"
utils::write.csv(release_index, file.path(out, "nbs-migrant-history-release-index.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8")

schemas <- unique(observations[c("schema_version", "release_period")])
dimensions <- rbind(
  data.frame(
    manifest_version = version, dataset_id = dataset_id, dimension = "schema_version",
    value = schemas$schema_version, label_en = paste("Native NBS release schema", schemas$release_period),
    label_zh = paste0(schemas$release_period, "年农民工监测调查原始表结构"),
    route_value = schemas$schema_version, sort_order = seq_len(nrow(schemas)),
    valid_from = paste0(schemas$release_period, "-01-01"), valid_to = paste0(schemas$release_period, "-12-31"),
    unit_en = "", unit_zh = "", semantics = "native_release_schema", stringsAsFactors = FALSE
  ),
  data.frame(
    manifest_version = observations$manifest_version, dataset_id = observations$dataset_id,
    dimension = "measure", value = observations$measure,
    label_en = observations$dimension_label_en, label_zh = observations$dimension_label_zh,
    route_value = observations$measure, sort_order = seq_len(nrow(observations)),
    valid_from = paste0(observations$release_period, "-01-01"),
    valid_to = paste0(observations$release_period, "-12-31"),
    unit_en = observations$unit_en, unit_zh = observations$unit_zh,
    semantics = "published_table_cell", stringsAsFactors = FALSE
  )
)
utils::write.csv(dimensions, file.path(out, "nbs-migrant-history-dataset-dimensions.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8")

manifest <- data.frame(
  manifest_version = version, dataset_id = dataset_id,
  release_id = "cn_nbs_migrant_workers_history", family = "migrant_workers",
  title_en = "Annual migrant-worker monitoring report archive (native tables)",
  title_zh = "年度农民工监测调查报告档案（原始表）",
  description_en = "Every numeric cell from retained official NBS 2012-24 monitoring reports, with each native release schema kept separate.",
  description_zh = "保留的国家统计局2012—2024年监测调查报告中全部数值单元，且各年度原始表结构独立保存。",
  frequency = "A", route = "press_html_table", default_start = "2008-01-01", available_to = "2024-12-31",
  coverage_en = "2012-24 official report captures; tables contain published retrospective observations from 2008, but their first-release reports are not bundled.",
  coverage_zh = "2012—2024年官方报告抓取；表中包含自2008年起的已发布回溯数值，但未捆绑其首次发布报告。",
  required_dimensions = "schema_version|measure", panel_allowed = "TRUE",
  expected_rows = nrow(observations), expected_cells = nrow(observations),
  aggregation_basis = "single_period", seasonal_adjustment = "not_applicable", derived_method = "none",
  source_vintage = "published_release_snapshot",
  series_note_en = "Selectors are release-cell identifiers; do not combine schemas as a backcast of the 2025 detailed dataset.",
  series_note_zh = "选择项为发布表单元标识；不得将不同表结构拼接为2025年详细数据集的回溯序列。",
  unit_en = "10,000 persons, yuan per person/worker per month, percent, percentage points, months per year, days per month or hours per day (cell-specific)",
  unit_zh = "万人、元/人/月、百分比、百分点、月/年、天/月或小时/天（按单元）",
  source_url = sources$source_url[[1L]],
  source_note = "Raw official captures and SHA-256 provenance are bundled; see docs/nbs-migrant-history.md.",
  period_policy = "all_periods", stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(out, "nbs-migrant-history-dataset-manifest.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat(sprintf("Built %d source-faithful migrant table cells across %d releases.\\n", nrow(observations), nrow(sources)))
