#!/usr/bin/env Rscript

# Offline maintenance generator for the bundled normalized NBS press archive.
# It deliberately consumes an explicit discovery manifest and already-downloaded
# pages. Package users never crawl the NBS release archive at fetch time.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Usage: update_nbs_press_archive.R TARGETS_CSV PAGES_DIR OUTPUT_DIR",
    call. = FALSE
  )
}
targets_path <- normalizePath(args[[1L]], mustWork = TRUE)
pages_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_dir <- normalizePath(args[[3L]], mustWork = TRUE)

pkgload::load_all(quiet = TRUE, export_all = FALSE)
manifest_version <- "2026-08-03.1"

targets <- utils::read.csv(
  targets_path, stringsAsFactors = FALSE, check.names = FALSE
)
targets$file <- file.path(pages_dir, basename(targets$file))
missing_pages <- targets$file[!file.exists(targets$file)]
if (length(missing_pages)) {
  stop(
    sprintf(
      "The explicit target manifest is missing %d downloaded page(s): %s",
      length(missing_pages), paste(basename(missing_pages), collapse = ", ")
    ),
    call. = FALSE
  )
}
migrant_file <- file.path(pages_dir, "migrant_t20260430_1963472.html")
migrant_url <- "https://www.stats.gov.cn/sj/zxfb/202604/t20260430_1963472.html"
if (file.exists(migrant_file) && file.info(migrant_file)$size > 1000L &&
    !migrant_url %in% targets$href) {
  targets <- rbind(targets, data.frame(
    page = "manual-current",
    title = "Migrant Workers Monitoring Survey Report 2025",
    href = migrant_url,
    kind = "migrant",
    file = migrant_file,
    stringsAsFactors = FALSE
  ))
}

bind_rows <- function(rows) {
  rows <- Filter(function(x) !is.null(x) && nrow(x), rows)
  if (!length(rows)) return(NULL)
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    for (column in setdiff(columns, names(row))) row[[column]] <- ""
    row[columns]
  })
  do.call(rbind, rows)
}

last_match <- function(pattern, text) {
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (!length(matches) || identical(matches, character(0)) || matches[[1L]] == "") {
    return(NA_character_)
  }
  tail(matches, 1L)
}

release_date_from_url <- function(url) {
  token <- last_match("t[0-9]{8}", url)
  as.Date(sub("^t", "", token), format = "%Y%m%d")
}

month_from_title <- function(title) {
  lower <- tolower(title)
  ordinal_months <- c(
    two = 2L, three = 3L, four = 4L, five = 5L, six = 6L,
    seven = 7L, eight = 8L, nine = 9L, ten = 10L, eleven = 11L
  )
  for (word in names(ordinal_months)) {
    if (grepl(paste("first", word, "months"), lower, fixed = TRUE)) {
      return(ordinal_months[[word]])
    }
  }
  if (grepl("first three quarters", lower, fixed = TRUE)) return(9L)
  if (grepl("first half", lower, fixed = TRUE)) return(6L)
  if (grepl("first quarter", lower, fixed = TRUE) ||
      grepl("jan-mar", lower, fixed = TRUE)) return(3L)
  if (grepl("jan-feb", lower, fixed = TRUE)) return(2L)
  hits <- which(vapply(tolower(month.name), function(month) {
    grepl(paste0("\\b", month, "\\b"), lower, perl = TRUE)
  }, logical(1)))
  if (!length(hits)) {
    hits <- which(vapply(tolower(month.abb), function(month) {
      grepl(paste0("\\b", month, "\\b"), lower, perl = TRUE)
    }, logical(1)))
  }
  if (!length(hits)) NA_integer_ else tail(hits, 1L)
}

period_metadata <- function(kind, title, url) {
  release_date <- release_date_from_url(url)
  year_text <- last_match("(?:19|20)[0-9]{2}", title)
  year <- suppressWarnings(as.integer(year_text))
  if (kind %in% c("wages", "demography", "migrant")) {
    if (is.na(year)) year <- as.integer(format(release_date, "%Y")) - 1L
    return(list(
      period = as.character(year),
      start = as.Date(sprintf("%04d-01-01", year)),
      end = as.Date(sprintf("%04d-12-31", year)),
      release_date = release_date
    ))
  }
  if (kind == "tenday") {
    pattern <- paste0(
      "([A-Za-z]+) ([0-9]{1,2})-([0-9]{1,2}), ((?:19|20)[0-9]{2})"
    )
    match <- regmatches(title, regexec(pattern, title, perl = TRUE))[[1L]]
    if (length(match) != 5L) stop("Unparsed ten-day title: ", title)
    month <- match(tolower(match[[2L]]), tolower(month.name))
    start_day <- as.integer(match[[3L]])
    end_day <- as.integer(match[[4L]])
    year <- as.integer(match[[5L]])
    start <- as.Date(sprintf("%04d-%02d-%02d", year, month, start_day))
    end <- as.Date(sprintf("%04d-%02d-%02d", year, month, end_day))
    dekad <- if (start_day == 1L) "D1" else if (start_day == 11L) "D2" else "D3"
    return(list(
      period = sprintf("%04d-%02d-%s", year, month, dekad),
      start = start, end = end, release_date = release_date
    ))
  }
  if (kind == "capacity") {
    quarter_words <- c(first = 1L, second = 2L, third = 3L, fourth = 4L)
    lower <- tolower(title)
    quarter <- quarter_words[vapply(names(quarter_words), function(word) {
      grepl(paste(word, "quarter"), lower, fixed = TRUE)
    }, logical(1L))]
    if (length(quarter) != 1L || is.na(year)) {
      stop("Unparsed capacity-utilisation title: ", title)
    }
    start_month <- (quarter[[1L]] - 1L) * 3L + 1L
    start <- as.Date(sprintf("%04d-%02d-01", year, start_month))
    end <- seq(start, by = "month", length.out = 4L)[[4L]] - 1
    return(list(
      period = sprintf("%04d-Q%d", year, quarter[[1L]]),
      start = start, end = end, release_date = release_date
    ))
  }
  month <- month_from_title(title)
  if (is.na(year)) {
    year <- as.integer(format(release_date, "%Y"))
    release_month <- as.integer(format(release_date, "%m"))
    if (!is.na(month) && month > release_month) year <- year - 1L
  }
  if (!is.na(year) && is.na(month) &&
      kind %in% c("industrial", "property", "retail", "households")) {
    month <- 12L
  }
  if (is.na(year) || is.na(month)) stop("Unparsed monthly title: ", title)
  end <- seq(as.Date(sprintf("%04d-%02d-01", year, month)), by = "month", length.out = 2L)[2L] - 1
  start <- if (kind == "housing") {
    as.Date(sprintf("%04d-%02d-01", year, month))
  } else {
    as.Date(sprintf("%04d-01-01", year))
  }
  period <- if (kind == "households") {
    sprintf("%04d-Q%d", year, month %/% 3L)
  } else {
    sprintf("%04d-%02d", year, month)
  }
  list(period = period, start = start, end = end, release_date = release_date)
}

datasets_for_kind <- list(
  industrial = c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY"
  ),
  housing = "NBS/CN/M/HOUSING_PRICES/70_CITY",
  property = c(
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
    "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION"
  ),
  retail = "NBS/CN/M/RETAIL_SALES/DETAIL",
  households = "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION",
  capacity = "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION",
  tenday = "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
  wages = "NBS/CN/A/EMPLOYMENT_WAGES",
  demography = "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
  migrant = "NBS/CN/A/MIGRANT_WORKERS"
)

ten_day_tuple_key <- function(product, specification, unit) {
  readNBS:::.nbs_press_code(paste(product, specification, unit, sep = " | "))
}
ten_day_identity_token <- function(value) {
  value <- tolower(trimws(as.character(value)))
  value <- gsub("[，、（）]", "", value, perl = TRUE)
  value <- gsub("[[:space:][:punct:]]+", "", value, perl = TRUE)
  value
}
ten_day_identity_key <- function(product, specification, unit) {
  paste(
    ten_day_identity_token(product),
    ten_day_identity_token(specification),
    ten_day_identity_token(unit),
    sep = "|"
  )
}
current_ten_day <- readNBS:::.nbs_load_core_press_observations()
current_ten_day <- current_ten_day[
  current_ten_day$dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES" &
    current_ten_day$measure == "price_yuan" &
    current_ten_day$product_code != "summary",
  ,
  drop = FALSE
]
current_ten_day <- current_ten_day[
  current_ten_day$release_period == max(current_ten_day$release_period),
  ,
  drop = FALSE
]
current_ten_day$key <- mapply(
  ten_day_tuple_key,
  current_ten_day$row_label,
  current_ten_day$specification,
  current_ten_day$unit,
  USE.NAMES = FALSE
)
if (nrow(current_ten_day) != 50L || anyDuplicated(current_ten_day$key)) {
  stop("The current ten-day product identity map is not unique.", call. = FALSE)
}
current_ten_day$identity_key <- mapply(
  ten_day_identity_key,
  current_ten_day$row_label,
  current_ten_day$specification,
  current_ten_day$unit,
  USE.NAMES = FALSE
)

# The pre-2024 official archive is Chinese-only. Map only bilingual tuples
# whose product, exact specification and physical unit are semantically
# continuous with the current codes. Real specification and basket changes
# remain separate and are deliberately not backcast through ordinal row IDs.
ten_day_aliases <- data.frame(
  product_code = c(
    "p001", "p002", "p003", "p004", "p005", "p006", "p007", "p008",
    "p009", "p010", "p011", "p012", "p012", "p013", "p014", "p016",
    "p017", "p019", "p020", "p022", "p023", "p024", "p025", "p026",
    "p027", "p028", "p029", "p030", "p031", "p034", "p035", "p036",
    "p037", "p038", "p039", "p040", "p041", "p046", "p047", "p047",
    "p048", "p049", "p050",
    "p026", "p034", "p035", "p036", "p037", "p038", "p039", "p041",
    "p050"
  ),
  product = c(
    "螺纹钢", "线材", "普通中板", "热轧普通板卷", "无缝钢管", "角钢",
    "电解铜", "铝锭", "铅锭", "锌锭", "硫酸", "烧碱（液碱）",
    "烧碱(液碱)", "甲醇", "纯苯（石油苯）", "聚乙烯（LLDPE）",
    "聚丙烯", "顺丁胶", "涤纶长丝", "液化天然气", "液化石油气", "汽油",
    "柴油", "石蜡", "无烟煤", "山西优混", "焦煤", "焦炭",
    "普通硅酸盐水泥", "稻米", "小麦", "玉米", "棉花（皮棉）", "生猪",
    "大豆", "豆粕", "花生", "复合肥", "农药（草甘膦）", "农药(草甘膦）",
    "天然橡胶", "纸浆", "瓦楞纸",
    "Paraffin", "Rice", "Wheat", "Corn", "Cotton (Ginned Cotton)",
    "Live Pig", "Soybean", "Peanut", "Corrugated Paper"
  ),
  specification = c(
    "Φ20mm,HRB400E", "Φ8-10mm,HPB300", "20mm,Q235",
    "4.75-11.5mm，Q235", "219*6,20#", "5#", "1#", "A00", "1#", "0#",
    "98%", "32%", "32%", "优等品", "工业级", "熔融指数2薄膜料", "拉丝料",
    "BR9000", "POY150D/48F", "LNG", "LPG", "95#国VI", "0#国VI", "58#半",
    "洗中块", "5500大卡", "主焦煤", "准一级冶金焦", "P.O 42.5散装",
    "粳稻米", "国标三等", "黄玉米二等", "白棉三级", "外三元", "黄豆",
    "粗蛋白含量≥43%", "油料花生米", "硫酸钾复合肥", "95%原药", "95%原药",
    "标准胶SCRWF", "进口针叶浆", "AA级120g",
    "58 # and A Half", "First-Class Japonica Rice",
    "Third-Class of National Standard)", "Second-Class of Yellow Corn",
    "Third-Class of White Cotton", "External Triple Crossbreed", "Soya",
    "Oil Peanut", "AA Class 120g"
  ),
  unit = rep("Yuan per tonne", 52L),
  stringsAsFactors = FALSE
)
ten_day_aliases$unit[
  ten_day_aliases$product %in% c("生猪", "Live Pig")
] <- "Yuan per kilogram"
ten_day_aliases$key <- mapply(
  ten_day_identity_key,
  ten_day_aliases$product,
  ten_day_aliases$specification,
  ten_day_aliases$unit,
  USE.NAMES = FALSE
)
ten_day_aliases <- ten_day_aliases[
  !duplicated(ten_day_aliases$key), , drop = FALSE
]
if (anyDuplicated(ten_day_aliases$key)) {
  stop("The historical ten-day alias whitelist is not unique.", call. = FALSE)
}

dataset_manifest <- readNBS:::.nbs_load_dataset_manifest()
observations <- list()
releases <- list()
failures <- list()
expected_targets <- list()
successful_targets <- list()

for (target_index in seq_len(nrow(targets))) {
  target <- targets[target_index, , drop = FALSE]
  target_datasets <- datasets_for_kind[[target$kind]]
  if (is.null(target_datasets)) {
    stop(sprintf("Unknown release kind `%s`.", target$kind), call. = FALSE)
  }
  for (dataset_id in target_datasets) {
    expected_targets[[length(expected_targets) + 1L]] <- data.frame(
      dataset_id = dataset_id,
      source_url = target$href,
      title = target$title,
      stringsAsFactors = FALSE
    )
  }
  if (target_index == 1L || target_index %% 25L == 0L) {
    cat(sprintf(
      "Parsing release target %d of %d (%s).\n",
      target_index, nrow(targets), target$kind[[1L]]
    ))
    flush.console()
  }
  metadata <- tryCatch(
    period_metadata(target$kind, target$title, target$href),
    error = function(error) error
  )
  if (inherits(metadata, "error")) {
    failures[[length(failures) + 1L]] <- data.frame(
      kind = target$kind,
      dataset_id = "",
      title = target$title,
      error = conditionMessage(metadata),
      stringsAsFactors = FALSE
    )
    next
  }
  html <- paste(readLines(target$file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  source_capture_sha256 <- digest::digest(
    file = target$file, algo = "sha256", serialize = FALSE
  )
  publication_matches <- regmatches(
    html,
    gregexpr(
      "20[0-9]{2}[-/][0-9]{2}[-/][0-9]{2}[[:space:]]+[0-9]{2}:[0-9]{2}",
      html,
      perl = TRUE
    )
  )[[1L]]
  if (length(publication_matches) && nzchar(publication_matches[[1L]])) {
    parsed_release_date <- as.Date(gsub(
      "/", "-", substr(publication_matches[[1L]], 1L, 10L), fixed = TRUE
    ))
    url_release_date <- release_date_from_url(target$href)
    # NBS occasionally republishes an English page under a URL dated in the
    # following month. The Chinese archive also migrated many older releases
    # into 2023 URL paths while preserving their original printed timestamp.
    migrated_legacy_page <- grepl(
      "/sj/.*/(?:202302|202303)/", target$href, perl = TRUE
    ) && parsed_release_date < url_release_date
    if (abs(as.integer(parsed_release_date - url_release_date)) > 62L &&
        !migrated_legacy_page) {
      stop(
        sprintf("Publication timestamp conflicts with URL for `%s`.", target$title),
        call. = FALSE
      )
    }
    metadata$release_date <- parsed_release_date
  }
  if (metadata$release_date < metadata$end) {
    stop(
      sprintf(
        "Release date %s precedes reference-period end %s for `%s`.",
        metadata$release_date, metadata$end, target$title
      ),
      call. = FALSE
    )
  }
  for (dataset_id in target_datasets) {
    dataset <- dataset_manifest[
      dataset_manifest$dataset_id == dataset_id, , drop = FALSE
    ]
    index_base <- if (target$kind == "housing") {
      housing_year <- as.integer(format(metadata$end, "%Y"))
      if (housing_year >= 2026L) {
        "2025"
      } else if (housing_year >= 2021L) {
        "2020"
      } else if (housing_year >= 2016L) {
        "2015"
      } else {
        "2010"
      }
    } else {
      ""
    }
    release_stub <- data.frame(
      period = metadata$period,
      reference_period_start = format(metadata$start),
      reference_period_end = format(metadata$end),
      release_date = format(metadata$release_date),
      source_url = target$href,
      index_base = index_base,
      stringsAsFactors = FALSE
    )
    parsed <- tryCatch(
      readNBS:::.nbs_parse_press_release(html, dataset, release_stub),
      error = function(error) error
    )
    if (inherits(parsed, "error")) {
      failures[[length(failures) + 1L]] <- data.frame(
        kind = target$kind,
        dataset_id = dataset_id,
        title = target$title,
        error = conditionMessage(parsed),
        stringsAsFactors = FALSE
      )
      next
    }
    if (target$kind == "retail") {
      is_sa <- parsed$measure == "mom_percent_official_sa"
      target_month <- as.Date(format(metadata$end, "%Y-%m-01"))
      target_sa <- is_sa & !is.na(parsed$observation_date) &
        parsed$observation_date == target_month
      if (sum(target_sa) != 1L) {
        failures[[length(failures) + 1L]] <- data.frame(
          kind = target$kind,
          dataset_id = dataset_id,
          title = target$title,
          error = sprintf(
            paste(
              "Retail release contained %d official SA rows for target month",
              "%s; expected exactly one."
            ),
            sum(target_sa), format(target_month, "%Y-%m")
          ),
          stringsAsFactors = FALSE
        )
        next
      }
      parsed <- parsed[
        !is_sa | target_sa,
        ,
        drop = FALSE
      ]
    }
    if (target$kind == "housing") {
      parsed_bases <- unique(parsed$index_base[nzchar(parsed$index_base)])
      if (length(parsed_bases) == 1L) index_base <- parsed_bases[[1L]]
    }
    if (target$kind == "wages") {
      observation_year <- as.integer(metadata$period) -
        as.integer(parsed$measure == "prior_year_average_wage")
      parsed$observation_date <- as.Date(sprintf(
        "%04d-01-01", observation_year
      ))
    }
    if (target$kind == "tenday" && format(metadata$start, "%Y") != "2026") {
      is_product <- parsed$product_code != "summary"
      parsed_keys <- mapply(
        ten_day_identity_key,
        parsed$row_label,
        parsed$specification,
        parsed$unit,
        USE.NAMES = FALSE
      )
      mapped_codes <- current_ten_day$product_code[
        match(parsed_keys, current_ten_day$identity_key)
      ]
      alias_match <- match(parsed_keys, ten_day_aliases$key)
      mapped_codes[is.na(mapped_codes)] <- ten_day_aliases$product_code[
        alias_match[is.na(mapped_codes)]
      ]
      keep <- !is_product | !is.na(mapped_codes)
      parsed <- parsed[keep, , drop = FALSE]
      parsed$product_code[is_product[keep]] <- mapped_codes[keep][is_product[keep]]
      canonical <- match(parsed$product_code, current_ten_day$product_code)
      canonical_product <- !is.na(canonical)
      parsed$row_label[canonical_product] <- current_ten_day$row_label[
        canonical[canonical_product]
      ]
      parsed$specification[canonical_product] <- current_ten_day$specification[
        canonical[canonical_product]
      ]
      parsed$unit[canonical_product] <- current_ten_day$unit[
        canonical[canonical_product]
      ]
      parsed$category[canonical_product] <- current_ten_day$category[
        canonical[canonical_product]
      ]
    }
    expected_cells <- switch(
      dataset_id,
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" = 112L,
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY" = 252L,
      "NBS/CN/M/HOUSING_PRICES/70_CITY" = if (
        format(metadata$end, "%Y") == "2011"
      ) 420L else if (
        format(metadata$end, "%m") == "01"
      ) 1120L else 1680L,
      "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL" = 7L,
      "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION" = 32L,
      "NBS/CN/M/RETAIL_SALES/DETAIL" = if (
        format(metadata$end, "%m") == "02"
      ) 53L else 105L,
      "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION" = 6L,
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES" = 153L,
      "NBS/CN/A/EMPLOYMENT_WAGES" = 315L,
      "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE" = 5L,
      "NBS/CN/A/MIGRANT_WORKERS" = 139L,
      nrow(parsed)
    )
    compatible_cardinality <- if (
      dataset_id == "NBS/CN/M/RETAIL_SALES/DETAIL"
    ) {
      nrow(parsed) >= 45L && nrow(parsed) <= 105L
    } else if (
      dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY"
    ) {
      nrow(parsed) %in% c(420L, 1120L, 1680L)
    } else if (
      dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION"
    ) {
      nrow(parsed) %in% c(24L, 32L)
    } else if (
      dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL"
    ) {
      nrow(parsed) %in% c(2L, 4L, 6L, 7L)
    } else if (
      dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES"
    ) {
      nrow(parsed) %in% c(307L, 315L)
    } else if (
      dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
    ) {
      nrow(parsed) >= 30L && nrow(parsed) <= 153L
    } else {
      nrow(parsed) == expected_cells
    }
    if (!compatible_cardinality) {
      failures[[length(failures) + 1L]] <- data.frame(
        kind = target$kind,
        dataset_id = dataset_id,
        title = target$title,
        error = sprintf(
          "Parsed %d cells; expected compatible schema cardinality %d.",
          nrow(parsed), expected_cells
        ),
        stringsAsFactors = FALSE
      )
      next
    }
    parsed$manifest_version <- manifest_version
    parsed$release_period <- metadata$period
    parsed$release_source_url <- target$href
    schema_version <- switch(
      target$kind,
      industrial = "industrial-gbt4754-2017-v1",
      housing = if (nrow(parsed) == 420L) {
        "housing-headline-fixed-base-v1"
      } else if (nrow(parsed) == 1120L) {
        "housing-two-comparison-v1"
      } else if ("fixed_base" %in% parsed$comparison) {
        "housing-fixed-base-v1"
      } else {
        "housing-ytd-average-v1"
      },
      property = if (
        dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL"
      ) {
        "property-national-supplement-v1"
      } else if (nrow(parsed) == 24L) {
        "property-three-region-legacy-v1"
      } else {
        "property-four-region-current-v1"
      },
      retail = if (grepl("-02$", metadata$period)) "retail-ytd-february-v1" else "retail-current-ytd-v1",
      households = "households-real-brackets-v1",
      capacity = "industrial-capacity-gbt4754-2017-v1",
      tenday = paste0(
        "means-production-current-products-in-basket-",
        format(metadata$start, "%Y"), "-v1"
      ),
      wages = if (nrow(parsed) == 307L) {
        "wages-2023-registration-transition-v1"
      } else {
        "wages-2024-plus-v1"
      },
      demography = if ("age_15_59_year_end" %in% parsed$measure) {
        "demography-age-15-59-2012-v1"
      } else {
        "demography-age-16-59-2013-plus-v1"
      },
      migrant = "migrant-monitoring-2025-v1"
    )
    basket_version <- switch(
      target$kind,
      industrial = "GB/T4754-2017",
      housing = paste0("housing-price-base-", index_base),
      property = if (
        dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL"
      ) {
        "national-property-current"
      } else if (nrow(parsed) == 24L) {
        "three-region-legacy"
      } else {
        "four-region-current"
      },
      retail = "retail-current-classification",
      households = "unified-household-survey-2015-plus",
      capacity = "GB/T4754-2017",
      tenday = if (format(metadata$start, "%Y") == "2020") {
        "means-of-production-basket-2020"
      } else if (format(metadata$start, "%Y") == "2021") {
        "means-of-production-basket-2021"
      } else if (format(metadata$start, "%Y") == "2022") {
        "means-of-production-basket-2022"
      } else if (format(metadata$start, "%Y") %in% c("2023", "2024", "2025")) {
        "means-of-production-basket-2023"
      } else {
        "means-of-production-basket-2026"
      },
      wages = "wage-registration-2023-occupation-2022",
      demography = paste0("population-communique-", format(metadata$end, "%Y")),
      migrant = "migrant-monitoring-2025"
    )
    parsed$schema_version <- schema_version
    parsed$basket_version <- basket_version
    observations[[length(observations) + 1L]] <- parsed
    releases[[length(releases) + 1L]] <- data.frame(
      manifest_version = manifest_version,
      dataset_id = dataset_id,
      period = metadata$period,
      reference_period_start = format(metadata$start),
      reference_period_end = format(metadata$end),
      release_date = format(metadata$release_date),
      source_url = target$href,
      basket_version = basket_version,
      index_base = index_base,
      expected_press_cells = nrow(parsed),
      schema_version = schema_version,
      storage = "snapshot",
      snapshot_sha256 = "PENDING",
      source_capture_sha256 = source_capture_sha256,
      stringsAsFactors = FALSE
    )
    successful_targets[[length(successful_targets) + 1L]] <- data.frame(
      dataset_id = dataset_id,
      source_url = target$href,
      stringsAsFactors = FALSE
    )
  }
}

observations <- bind_rows(observations)
releases <- bind_rows(releases)
failures <- bind_rows(failures)
if (is.null(observations) || is.null(releases)) stop("No releases parsed.")

expected_targets <- bind_rows(expected_targets)
successful_targets <- bind_rows(successful_targets)
expected_keys <- paste(
  expected_targets$dataset_id, expected_targets$source_url, sep = "\r"
)
successful_keys <- paste(
  successful_targets$dataset_id, successful_targets$source_url, sep = "\r"
)
missing_keys <- !expected_keys %in% successful_keys
if (any(missing_keys)) {
  missing_targets <- expected_targets[missing_keys, , drop = FALSE]
  failures <- bind_rows(list(
    failures,
    data.frame(
      kind = "target_coverage",
      dataset_id = missing_targets$dataset_id,
      title = missing_targets$title,
      error = "Explicit target did not produce an indexed release.",
      stringsAsFactors = FALSE
    )
  ))
}

report_path <- Sys.getenv(
  "READNBS_ARCHIVE_REPORT",
  file.path(tempdir(), "nbs-press-archive-failures.csv")
)
utils::write.csv(
  if (is.null(failures)) {
    data.frame(
      kind = character(), dataset_id = character(), title = character(),
      error = character()
    )
  } else {
    failures
  },
  report_path,
  row.names = FALSE,
  na = ""
)
if (!is.null(failures) && nrow(failures)) {
  stop(
    sprintf(
      "Archive generation aborted with %d failure(s); see %s.",
      nrow(failures), report_path
    ),
    call. = FALSE
  )
}

observations <- observations[
  order(observations$dataset_id, observations$release_period,
        observations$table_index, observations$row_index,
        observations$column_index),
  ,
  drop = FALSE
]
releases <- releases[
  order(
    releases$dataset_id, releases$reference_period_start,
    releases$release_date
  ),
  ,
  drop = FALSE
]
release_key <- paste(releases$dataset_id, releases$period, sep = "\r")
if (anyDuplicated(release_key)) {
  releases <- releases[!duplicated(release_key, fromLast = TRUE), , drop = FALSE]
  selected <- paste(
    releases$dataset_id, releases$period, releases$source_url, sep = "\r"
  )
  observation_release <- paste(
    observations$dataset_id, observations$release_period,
    observations$release_source_url, sep = "\r"
  )
  observations <- observations[
    observation_release %in% selected, , drop = FALSE
  ]
}

observation_release_key <- paste(
  observations$dataset_id,
  observations$release_period,
  observations$release_source_url,
  sep = "\r"
)
indexed_release_key <- paste(
  releases$dataset_id, releases$period, releases$source_url, sep = "\r"
)
matched_release <- match(observation_release_key, indexed_release_key)
if (anyNA(matched_release) ||
    any(observations$schema_version != releases$schema_version[matched_release]) ||
    any(observations$basket_version != releases$basket_version[matched_release])) {
  stop(
    "Archived observations must carry their indexed schema and basket versions.",
    call. = FALSE
  )
}

archive_path <- file.path(output_dir, "nbs-press-observations.csv.gz")
connection <- gzfile(archive_path, open = "wt", encoding = "UTF-8")
utils::write.csv(observations, connection, row.names = FALSE, na = "")
close(connection)
archive_hash <- digest::digest(
  file = archive_path, algo = "sha256", serialize = FALSE
)
releases$snapshot_sha256 <- archive_hash
utils::write.csv(
  releases,
  file.path(output_dir, "nbs-release-index.csv"),
  row.names = FALSE,
  na = ""
)
cat(
  sprintf(
    "Wrote %d observations across %d releases; %d parser failures.\n",
    nrow(observations), nrow(releases), if (is.null(failures)) 0L else nrow(failures)
  )
)
cat("Parser report: ", report_path, "\n", sep = "")
