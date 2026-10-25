#!/usr/bin/env Rscript

# Build the isolated 2014--19 ten-day production-market supplement from frozen
# official NBS Chinese pages.  Product identity is always the exact published
# product/specification/physical-unit tuple; no ordinal table position is used
# to bridge a basket change.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: build_nbs_tenday_history.R TARGETS_CSV CAPTURES_DIR OUTPUT_DIR", call. = FALSE)
}
targets_path <- normalizePath(args[[1L]], mustWork = TRUE)
captures_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_dir <- normalizePath(args[[3L]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
targets <- utils::read.csv(targets_path, stringsAsFactors = FALSE, check.names = FALSE,
  colClasses = "character", fileEncoding = "UTF-8")
required <- c("period", "reference_period_start", "reference_period_end", "title", "href", "source_capture_file")
if (!nrow(targets) || any(!required %in% names(targets)) || anyDuplicated(targets$period)) {
  stop("Ten-day target manifest is invalid.", call. = FALSE)
}
targets$source_capture_file <- file.path(captures_dir, basename(targets$source_capture_file))
if (any(!file.exists(targets$source_capture_file))) stop("A frozen official capture is missing.", call. = FALSE)

sha256 <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
number <- function(x, context) {
  x <- trimws(as.character(x))
  x <- gsub(",", "", x, fixed = TRUE)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", x)) stop("Non-numeric ", context, ".", call. = FALSE)
  as.numeric(x)
}
physical_unit <- function(unit) {
  unit <- trimws(unit)
  switch(unit, "吨" = "tonne", "千克" = "kilogram", "张" = "sheet", unit)
}
physical_unit_title <- function(unit) {
  switch(unit, "吨" = "Tonne", "千克" = "Kilogram", "张" = "Sheet", unit)
}
product_names_en <- c(
  "螺纹钢" = "Rebar", "线材" = "Wire rod", "普通中板" = "Ordinary medium plate",
  "热轧普通薄板" = "Ordinary hot-rolled thin sheet", "无缝钢管" = "Seamless steel pipe",
  "角钢" = "Angle steel", "电解铜" = "Electrolytic copper", "铝锭" = "Aluminium ingot",
  "铅锭" = "Lead ingot", "锌锭" = "Zinc ingot", "硫酸" = "Sulfuric acid",
  "烧碱(液碱)" = "Caustic soda (liquid)", "甲醇" = "Methanol",
  "纯苯（石油苯）" = "Pure benzene (petroleum benzene)", "苯乙烯" = "Styrene",
  "聚乙烯（LLDPE）" = "Polyethylene (LLDPE)", "聚丙烯" = "Polypropylene",
  "聚氯乙烯" = "Polyvinyl chloride", "顺丁胶" = "Butadiene rubber",
  "涤纶长丝" = "Polyester filament", "液化天然气" = "Liquefied natural gas",
  "液化石油气" = "Liquefied petroleum gas", "汽油" = "Gasoline", "柴油" = "Diesel oil",
  "石蜡" = "Paraffin wax", "无烟煤" = "Anthracite", "普通混煤" = "Ordinary mixed coal",
  "山西大混" = "Shanxi mixed coal", "山西优混" = "Shanxi premium mixed coal",
  "大同混煤" = "Datong mixed coal", "焦煤(1/3焦煤)" = "Coking coal (1/3 coking coal)",
  "焦炭" = "Coke", "复合硅酸盐水泥" = "Composite Portland cement",
  "普通硅酸盐水泥" = "Ordinary Portland cement", "浮法平板玻璃" = "Float flat glass",
  "稻米" = "Rice", "小麦" = "Wheat", "玉米" = "Corn", "棉花（皮棉）" = "Cotton (ginned)",
  "生猪" = "Live hog", "大豆" = "Soybean", "豆粕" = "Soybean meal", "花生" = "Peanut",
  "尿素" = "Urea", "复合肥" = "Compound fertilizer",
  "农药(草甘膦）" = "Pesticide (glyphosate)", "人造板" = "Wood-based panel",
  "纸浆" = "Pulp", "瓦楞纸" = "Corrugated paper"
)
specifications_en <- c(
  "优等品" = "Superior grade", "工业级" = "Industrial grade", "一级品" = "Grade I",
  "58#半" = "58#, semi-refined", "2号洗中块" = "No. 2 washed medium lump",
  "4500大卡" = "4,500 kcal", "5000大卡" = "5,000 kcal", "5500大卡" = "5,500 kcal",
  "5800大卡" = "5,800 kcal", "1/3焦煤" = "1/3 coking coal",
  "二级冶金焦" = "Grade II metallurgical coke", "P.C 32.5袋装" = "P.C 32.5, bagged",
  "P.O 42.5散装" = "P.O 42.5, bulk", "粳稻米" = "Japonica rice",
  "国标三等" = "National standard grade III", "黄玉米二等" = "Yellow corn grade II",
  "白棉三级" = "White cotton grade III", "外三元" = "Three-way crossbred",
  "黄豆" = "Yellow soybean", "粗蛋白含量≥43%" = "Crude protein content ≥43%",
  "油料花生米" = "Oil-grade peanut kernels", "小颗料" = "Small granules",
  "硫酸钾复合肥" = "Potassium sulfate compound fertilizer",
  "95%原药" = "95% technical material", "漂白化学浆" = "Bleached chemical pulp",
  "高强" = "High strength", "粳稻米一级" = "Grade I japonica rice",
  "P.C 32.5R袋装" = "P.C 32.5R, bagged", "95#国Ⅴ" = "95 RON, China V",
  "92#国Ⅴ" = "92 RON, China V", "0#国Ⅴ" = "0#, China V",
  "95#国VI" = "95 RON, China VI", "92#国VI" = "92 RON, China VI",
  "0#国VI" = "0#, China VI", "P.O 42.5袋装" = "P.O 42.5, bagged"
)
categories_en <- c(
  "一、黑色金属" = "Ferrous metals", "二、有色金属" = "Non-ferrous metals",
  "三、化工产品" = "Chemical products", "四、石油天然气" = "Petroleum and natural gas",
  "五、煤炭" = "Coal", "六、非金属建材" = "Non-metallic building materials",
  "七、农产品" = "Agricultural products", "七、农产品（主要用于加工）" = "Agricultural products (mainly for processing)",
  "八、农业生产资料" = "Agricultural inputs", "九、林产品" = "Forest products"
)
translate_product <- function(value) {
  translated <- unname(product_names_en[value])
  if (is.na(translated) || !nzchar(translated)) stop("Missing product translation: ", value, call. = FALSE)
  translated
}
translate_specification <- function(value) {
  if (!grepl("[一-龥]", value)) return(value)
  translated <- unname(specifications_en[value])
  if (is.na(translated) || !nzchar(translated)) stop("Missing specification translation: ", value, call. = FALSE)
  translated
}
release_date <- function(html, source_url) {
  text <- rvest::html_text2(xml2::read_html(html))
  hit <- regmatches(text, regexpr("20[0-9]{2}[-/]?[0-9]{2}[-/]?[0-9]{2}", text, perl = TRUE))
  if (!length(hit) || !nzchar(hit)) stop("No printed publication date in official capture.", call. = FALSE)
  as.Date(gsub("/", "-", hit, fixed = TRUE))
}
tuple_code <- function(product, specification, source_product_unit) {
  raw <- paste(product, specification, source_product_unit, sep = " | ")
  paste0("tuple_", substr(digest::digest(raw, algo = "sha256", serialize = FALSE), 1L, 20L))
}
label_key <- function(value) {
  gsub("[[:space:][:punct:]（）《》，、]", "", trimws(value), perl = TRUE)
}

records <- list()
releases <- list()
for (i in seq_len(nrow(targets))) {
  path <- targets$source_capture_file[[i]]
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tables <- rvest::html_table(xml2::read_html(html), fill = TRUE)
  if (length(tables) < 2L || ncol(tables[[1L]]) < 5L || ncol(tables[[2L]]) < 3L) {
    stop("Official ten-day table layout was not found for ", targets$period[[i]], ".", call. = FALSE)
  }
  prices <- as.data.frame(tables[[1L]], stringsAsFactors = FALSE)
  specs <- as.data.frame(tables[[2L]], stringsAsFactors = FALSE)
  product_rows <- which(grepl("^[+-]?[0-9]", trimws(prices[[3L]])))
  # The specification table numbers its category headings as well as its 50
  # products, and some releases contain additional blank price-table rows.
  # A real specification row has a model/specification or explanatory note;
  # category headings have neither.
  spec_rows <- which(
    grepl("^[0-9]+$", trimws(specs[[1L]])) &
      (nzchar(trimws(specs[[3L]])) | nzchar(trimws(specs[[4L]])))
  )
  if (length(product_rows) != 50L || length(spec_rows) != 50L) {
    stop("Official release did not contain exactly 50 product tuples for ", targets$period[[i]], ".", call. = FALSE)
  }
  category <- ""
  product_number <- 0L
  release_rows <- list()
  for (row in seq_len(nrow(prices))) {
    if (!row %in% product_rows) {
      label <- trimws(prices[[1L]][[row]])
      if (nzchar(label) && row > 1L) category <- label
      next
    }
    product_number <- product_number + 1L
    # Align the 50 numeric price rows to the 50 non-heading specification rows.
    # Never count numbered category headings as products.
    spec_row <- spec_rows[[product_number]]
    product <- trimws(specs[[2L]][[spec_row]])
    specification <- trimws(specs[[3L]][[spec_row]])
    source_product_unit <- trimws(prices[[2L]][[row]])
    price_label <- trimws(prices[[1L]][[row]])
    if (!nzchar(product) || grepl("^[一二三四五六七八九十]+、", product) ||
        !startsWith(label_key(price_label), label_key(product))) {
      stop("Ten-day product/specification rows do not align for ",
        targets$period[[i]], " row ", row, ".", call. = FALSE)
    }
    product_code <- tuple_code(product, specification, source_product_unit)
    product_en <- translate_product(product)
    specification_en <- translate_specification(specification)
    category_en <- unname(categories_en[category])
    if (is.na(category_en) || !nzchar(category_en)) {
      stop("Missing category translation: ", category, call. = FALSE)
    }
    dimension_label_en <- paste0(product_en, " (", specification_en, ")")
    measures <- c("price_yuan", "change_yuan", "change_percent")
    for (column in 3:5) {
      raw <- trimws(prices[[column]][[row]])
      value <- number(raw, paste0("ten-day table cell ", targets$period[[i]]))
      release_rows[[length(release_rows) + 1L]] <- data.frame(
        dataset_id = "NBS/CN/10D/PRODUCTION_MARKET_PRICES", release_period = targets$period[[i]],
        category = category, product_code = product_code, product_identity = paste(product, specification, source_product_unit, sep = " | "),
        row_label = product, dimension_label_en = dimension_label_en,
        dimension_label_zh = price_label, category_en = category_en,
        product_label_en = product_en, specification = specification,
        specification_en = specification_en, source_product_unit = source_product_unit,
        measure = measures[[column - 2L]], period_basis = "ten_day", raw_value = raw, value = value,
        unit = if (column == 5L) "Percent" else paste("Yuan per", physical_unit(source_product_unit)),
        observation_date = targets$reference_period_start[[i]], observation_period_start = targets$reference_period_start[[i]],
        observation_period_end = targets$reference_period_end[[i]], table_index = 1L, row_index = row, column_index = column,
        schema_version = "means-production-basket-source-tuple-v1", basket_version = "means-of-production-basket-source-tuple",
        manifest_version = "2026-08-11.1", release_source_url = targets$href[[i]], stringsAsFactors = FALSE
      )
    }
  }
  body <- rvest::html_text2(xml2::read_html(html))
  counts <- regmatches(body, regexec("([0-9]+)种产品价格上涨[，,]\\s*([0-9]+)种下降[，,]\\s*([0-9]+)种持平", body, perl = TRUE))[[1L]]
  if (length(counts) != 4L) stop("Official direction counts not found for ", targets$period[[i]], ".", call. = FALSE)
  for (j in 1:3) {
    release_rows[[length(release_rows) + 1L]] <- data.frame(
      dataset_id = "NBS/CN/10D/PRODUCTION_MARKET_PRICES", release_period = targets$period[[i]],
      category = "summary", product_code = "summary", product_identity = "summary", row_label = "summary",
      dimension_label_en = "Published product-direction count", dimension_label_zh = "产品涨跌数量",
      category_en = "Summary", product_label_en = "Summary",
      specification = "not_applicable", specification_en = "not applicable", source_product_unit = "", measure = c("increased_product_count", "decreased_product_count", "flat_product_count")[[j]],
      period_basis = "ten_day", raw_value = counts[[j + 1L]], value = as.numeric(counts[[j + 1L]]), unit = "Products",
      observation_date = targets$reference_period_start[[i]], observation_period_start = targets$reference_period_start[[i]],
      observation_period_end = targets$reference_period_end[[i]], table_index = 0L, row_index = j, column_index = 1L,
      schema_version = "means-production-basket-source-tuple-v1", basket_version = "means-of-production-basket-source-tuple",
      manifest_version = "2026-08-11.1", release_source_url = targets$href[[i]], stringsAsFactors = FALSE
    )
  }
  parsed <- do.call(rbind, release_rows)
  if (nrow(parsed) != 153L || anyDuplicated(paste(parsed$product_identity, parsed$measure, sep = "\r"))) {
    stop("Ten-day cardinality or source-tuple uniqueness failed for ", targets$period[[i]], ".", call. = FALSE)
  }
  records[[length(records) + 1L]] <- parsed
  releases[[length(releases) + 1L]] <- data.frame(
    manifest_version = "2026-08-11.1", dataset_id = "NBS/CN/10D/PRODUCTION_MARKET_PRICES", period = targets$period[[i]],
    reference_period_start = targets$reference_period_start[[i]], reference_period_end = targets$reference_period_end[[i]],
    release_date = format(release_date(html, targets$href[[i]])), source_url = targets$href[[i]],
    basket_version = "means-of-production-basket-source-tuple", expected_press_cells = nrow(parsed),
    schema_version = "means-production-basket-source-tuple-v1", storage = "snapshot", snapshot_sha256 = "PENDING",
    source_capture_sha256 = sha256(path), stringsAsFactors = FALSE
  )
}
observations <- do.call(rbind, records)
archive <- file.path(output_dir, "nbs-tenday-history-observations.csv.gz")
connection <- gzfile(archive, "wt")
utils::write.csv(observations, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)
archive_hash <- sha256(archive)
index <- do.call(rbind, releases)
index$snapshot_sha256 <- archive_hash
utils::write.csv(index, file.path(output_dir, "nbs-tenday-history-release-index.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
product_rows <- observations[observations$product_code != "summary", , drop = FALSE]
products <- product_rows[!duplicated(product_rows$product_code), c(
  "product_code", "dimension_label_en", "dimension_label_zh", "source_product_unit",
  "specification_en"
)]
first_period <- tapply(product_rows$release_period, product_rows$product_code, min)
last_period <- tapply(product_rows$release_period, product_rows$product_code, max)
release_start <- stats::setNames(index$reference_period_start, index$period)
release_end <- stats::setNames(index$reference_period_end, index$period)
dimensions <- data.frame(
  manifest_version = "2026-08-11.1",
  dataset_id = "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
  dimension = "product_code", value = products$product_code,
  label_en = products$dimension_label_en, label_zh = products$dimension_label_zh,
  route_value = products$product_code, sort_order = seq_len(nrow(products)) + 100L,
  valid_from = unname(release_start[first_period[products$product_code]]),
  valid_to = unname(release_end[last_period[products$product_code]]),
  unit_en = vapply(products$source_product_unit, physical_unit_title, character(1L)),
  unit_zh = products$source_product_unit, semantics = products$specification_en,
  stringsAsFactors = FALSE
)
utils::write.csv(dimensions, file.path(output_dir, "nbs-tenday-history-dataset-dimensions.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("Wrote %d source-faithful cells across %d releases; archive SHA-256 %s\n", nrow(observations), nrow(index), archive_hash))
