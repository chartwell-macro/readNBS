.nbs_load_release_index <- function() {
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-release-index.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  # Housing history is deliberately distributed as a separate archive: its
  # 2011--18 Chinese-only releases use an earlier fixed-base methodology and
  # are maintained independently of the current multi-dataset press bundle.
  housing_index_path <- system.file(
    "extdata", "nbs-housing-history-release-index.csv",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(housing_index_path)) {
    candidate <- file.path("inst", "extdata", "nbs-housing-history-release-index.csv")
    if (file.exists(candidate)) housing_index_path <- candidate
  }
  if (nzchar(housing_index_path) && file.exists(housing_index_path)) {
    housing_index <- utils::read.csv(
      housing_index_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    index <- rbind(index, housing_index[names(index)])
  }
  wages_index_path <- system.file(
    "extdata", "nbs-wages-history-release-index.csv",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(wages_index_path)) {
    candidate <- file.path("inst", "extdata", "nbs-wages-history-release-index.csv")
    if (file.exists(candidate)) wages_index_path <- candidate
  }
  if (nzchar(wages_index_path) && file.exists(wages_index_path)) {
    wages_index <- utils::read.csv(
      wages_index_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    index <- rbind(index, wages_index[names(index)])
  }
  households_legacy_index_path <- system.file(
    "extdata", "nbs-households-legacy-release-index.csv",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(households_legacy_index_path)) {
    candidate <- file.path("inst", "extdata", "nbs-households-legacy-release-index.csv")
    if (file.exists(candidate)) households_legacy_index_path <- candidate
  }
  if (nzchar(households_legacy_index_path) && file.exists(households_legacy_index_path)) {
    households_legacy_index <- utils::read.csv(
      households_legacy_index_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    index <- rbind(index, households_legacy_index[names(index)])
  }
  migrant_history_index <- .nbs_load_migrant_history_release_index()
  index <- rbind(index, migrant_history_index[names(index)])
  capacity_history_index <- .nbs_load_capacity_history_release_index()
  index <- rbind(index, capacity_history_index[names(index)])
  demography_history_index <- .nbs_load_demography_history_release_index()
  index <- rbind(index, demography_history_index[names(index)])
  tenday_history_index <- .nbs_load_tenday_history_release_index()
  # The retained 2014--19 archive predates the index-base field used by the
  # housing release family.  Preserve the shared release-index contract.
  tenday_history_index$index_base <- ""
  index <- rbind(index, tenday_history_index[names(index)])
  longitudinal_index <- utils::read.csv(
    .nbs_extdata_path("nbs-longitudinal-release-index.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  index <- rbind(index, longitudinal_index[names(index)])
  required <- c(
    "manifest_version", "dataset_id", "period", "reference_period_start",
    "reference_period_end", "release_date", "source_url", "basket_version",
    "index_base", "expected_press_cells", "schema_version", "storage",
    "snapshot_sha256", "source_capture_sha256"
  )
  if (any(!required %in% names(index)) ||
      anyDuplicated(paste(index$dataset_id, index$period)) ||
      any(!index$storage %in% c("snapshot", "live_document")) ||
      any(!nzchar(index$schema_version)) ||
      any(!nzchar(index$snapshot_sha256[index$storage == "snapshot"])) ||
      any(!grepl("^[0-9a-f]{64}$", index$source_capture_sha256))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS release index is invalid."
    )
  }
  index$expected_press_cells <- suppressWarnings(
    as.integer(index$expected_press_cells)
  )
  if (anyNA(index$expected_press_cells) ||
      any(index$expected_press_cells < 1L)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS release index has invalid expected cell counts."
    )
  }
  index
}

.nbs_press_observation_cache <- new.env(parent = emptyenv())

.nbs_load_press_observations <- function() {
  if (exists("observations", envir = .nbs_press_observation_cache,
      inherits = FALSE)) {
    return(get(
      "observations", envir = .nbs_press_observation_cache,
      inherits = FALSE
    ))
  }
  archive_path <- .nbs_extdata_path("nbs-press-observations.csv.gz")
  observations <- utils::read.csv(
    archive_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  housing_archive_path <- system.file(
    "extdata", "nbs-housing-history-observations.csv.gz",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(housing_archive_path)) {
    candidate <- file.path("inst", "extdata", "nbs-housing-history-observations.csv.gz")
    if (file.exists(candidate)) housing_archive_path <- candidate
  }
  if (nzchar(housing_archive_path) && file.exists(housing_archive_path)) {
    housing_observations <- utils::read.csv(
      housing_archive_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    for (column in setdiff(names(observations), names(housing_observations))) {
      housing_observations[[column]] <- ""
    }
    observations <- rbind(observations, housing_observations[names(observations)])
  }
  wages_archive_path <- system.file(
    "extdata", "nbs-wages-history-observations.csv.gz",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(wages_archive_path)) {
    candidate <- file.path("inst", "extdata", "nbs-wages-history-observations.csv.gz")
    if (file.exists(candidate)) wages_archive_path <- candidate
  }
  if (nzchar(wages_archive_path) && file.exists(wages_archive_path)) {
    wages_observations <- utils::read.csv(
      wages_archive_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    for (column in setdiff(names(wages_observations), names(observations))) {
      observations[[column]] <- ""
    }
    for (column in setdiff(names(observations), names(wages_observations))) {
      wages_observations[[column]] <- ""
    }
    observations <- rbind(observations, wages_observations[names(observations)])
  }
  households_legacy_archive_path <- system.file(
    "extdata", "nbs-households-legacy-observations.csv.gz",
    package = "readNBS", mustWork = FALSE
  )
  if (!nzchar(households_legacy_archive_path)) {
    candidate <- file.path("inst", "extdata", "nbs-households-legacy-observations.csv.gz")
    if (file.exists(candidate)) households_legacy_archive_path <- candidate
  }
  if (nzchar(households_legacy_archive_path) && file.exists(households_legacy_archive_path)) {
    households_legacy_observations <- utils::read.csv(
      households_legacy_archive_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    )
    for (column in setdiff(names(households_legacy_observations), names(observations))) {
      observations[[column]] <- ""
    }
    for (column in setdiff(names(observations), names(households_legacy_observations))) {
      households_legacy_observations[[column]] <- ""
    }
    observations <- rbind(
      observations, households_legacy_observations[names(observations)]
    )
  }
  demography_history_observations <- .nbs_load_demography_history_observations()
  for (column in setdiff(names(demography_history_observations), names(observations))) {
    observations[[column]] <- ""
  }
  for (column in setdiff(names(observations), names(demography_history_observations))) {
    demography_history_observations[[column]] <- ""
  }
  observations <- rbind(
    observations, demography_history_observations[names(observations)]
  )
  required <- c(
    "manifest_version", "dataset_id", "release_period", "category",
    "industry", "city_code", "market", "size_band", "comparison",
    "index_base", "region", "measure", "period_basis", "residence",
    "nominal_real", "unit_type", "registration", "occupation",
    "product_code", "raw_value", "value", "observation_date"
  )
  if (any(!required %in% names(observations)) ||
      any(!is.finite(suppressWarnings(as.numeric(observations$value[
        nzchar(observations$value)
      ]))))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS press-observation archive is invalid."
    )
  }
  index <- .nbs_load_release_index()
  housing_history <- index$dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY" &
    index$reference_period_end < "2019-05-01"
  wages_history <- index$dataset_id %in% c(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY",
    "NBS/CN/A/WAGES/PRIVATE_HISTORY"
  )
  households_legacy_history <- index$dataset_id %in% c(
    "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
    "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012"
  )
  demography_history <- index$dataset_id == "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE" &
    index$reference_period_end < "2012-01-01"
  migrant_history <- index$dataset_id ==
    "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"
  capacity_history <- index$dataset_id %in% c(
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY",
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019"
  )
  expected_hashes <- unique(index$snapshot_sha256[
    index$storage == "snapshot" & !housing_history & !wages_history &
      !households_legacy_history & !demography_history & !migrant_history &
      !capacity_history
  ])
  actual_hash <- digest::digest(
    file = archive_path, algo = "sha256", serialize = FALSE
  )
  if (length(expected_hashes) != 1L || !identical(expected_hashes, actual_hash)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled NBS press-observation archive hash is invalid."
    )
  }
  if (nzchar(housing_archive_path) && file.exists(housing_archive_path)) {
    expected_housing_hashes <- unique(index$snapshot_sha256[
      index$storage == "snapshot" & housing_history
    ])
    actual_housing_hash <- digest::digest(
      file = housing_archive_path, algo = "sha256", serialize = FALSE
    )
    if (length(expected_housing_hashes) != 1L ||
        !identical(expected_housing_hashes, actual_housing_hash)) {
      .nbs_abort(
        "nbs_manifest_error",
        "The bundled NBS housing-history archive hash is invalid."
      )
    }
  }
  if (nzchar(wages_archive_path) && file.exists(wages_archive_path)) {
    expected_wages_hashes <- unique(index$snapshot_sha256[
      index$storage == "snapshot" & wages_history
    ])
    actual_wages_hash <- digest::digest(
      file = wages_archive_path, algo = "sha256", serialize = FALSE
    )
    if (length(expected_wages_hashes) != 1L ||
        !identical(expected_wages_hashes, actual_wages_hash)) {
      .nbs_abort(
        "nbs_manifest_error",
        "The bundled NBS wage-history archive hash is invalid."
      )
    }
  }
  if (nzchar(households_legacy_archive_path) && file.exists(households_legacy_archive_path)) {
    expected_households_legacy_hashes <- unique(index$snapshot_sha256[
      index$storage == "snapshot" & households_legacy_history
    ])
    actual_households_legacy_hash <- digest::digest(
      file = households_legacy_archive_path, algo = "sha256", serialize = FALSE
    )
    if (length(expected_households_legacy_hashes) != 1L ||
        !identical(expected_households_legacy_hashes, actual_households_legacy_hash)) {
      .nbs_abort(
        "nbs_manifest_error",
        "The bundled NBS household-legacy archive hash is invalid."
      )
    }
  }
  expected_demography_hashes <- unique(index$snapshot_sha256[
    index$storage == "snapshot" & demography_history
  ])
  actual_demography_hash <- digest::digest(
    file = .nbs_extdata_path("nbs-demography-history-observations.csv.gz"),
    algo = "sha256", serialize = FALSE
  )
  if (length(expected_demography_hashes) != 1L ||
      !identical(expected_demography_hashes, actual_demography_hash)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled early-demography archive hash is invalid."
    )
  }
  observations$value <- suppressWarnings(as.numeric(observations$value))
  for (column in intersect(
    c("table_index", "row_index", "column_index"), names(observations)
  )) {
    observations[[column]] <- suppressWarnings(as.integer(observations[[column]]))
  }
  observations$observation_date <- as.Date(ifelse(
    nzchar(observations$observation_date),
    observations$observation_date,
    NA_character_
  ))
  assign(
    "observations", observations,
    envir = .nbs_press_observation_cache
  )
  observations
}

.nbs_press_code <- function(x) {
  code <- toupper(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  code <- gsub("[^A-Z0-9]+", "_", code)
  code <- gsub("^_+|_+$", "", code)
  code <- gsub("_+", "_", code)
  if (is.na(code) || !nzchar(code)) {
    code <- paste0(
      "CODE_", toupper(substr(digest::digest(x, algo = "sha256"), 1L, 10L))
    )
  }
  tolower(code)
}

.nbs_panel_record <- function(
    dataset_id,
    raw_value,
    value,
    table_index,
    row_index,
    column_index,
    row_label = "",
    category = "",
    industry = "",
    city_code = "",
    city_label = "",
    market = "",
    size_band = "",
    comparison = "",
    index_base = "",
    region = "",
    measure = "",
    period_basis = "",
    residence = "",
    nominal_real = "",
    unit_type = "",
    registration = "",
    occupation = "",
    product_code = "",
    specification = "",
    unit = "",
    section = "",
    observation_date = as.Date(NA)) {
  data.frame(
    dataset_id = dataset_id,
    category = category,
    industry = industry,
    city_code = city_code,
    city_label = city_label,
    market = market,
    size_band = size_band,
    comparison = comparison,
    index_base = index_base,
    region = region,
    measure = measure,
    period_basis = period_basis,
    residence = residence,
    nominal_real = nominal_real,
    unit_type = unit_type,
    registration = registration,
    occupation = occupation,
    product_code = product_code,
    specification = specification,
    unit = unit,
    section = section,
    row_label = row_label,
    table_index = as.integer(table_index),
    row_index = as.integer(row_index),
    column_index = as.integer(column_index),
    raw_value = as.character(raw_value),
    value = as.numeric(value),
    observation_date = as.Date(observation_date),
    stringsAsFactors = FALSE
  )
}

.nbs_strict_number <- function(value, context) {
  raw <- trimws(as.character(value))
  normalized <- gsub(",", "", raw, fixed = TRUE)
  if (!grepl(
    "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", normalized
  )) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf("%s contains non-numeric value `%s`.", context, raw)
    )
  }
  list(raw = raw, value = as.numeric(normalized))
}

.nbs_number_or_published_missing <- function(value, context) {
  raw <- trimws(as.character(value))
  if (is.na(raw) || !nzchar(raw) ||
      raw %in% c("NA", "-", "--", "\u2014", "\u2026") ||
      grepl("^[\uFF08(](?:\u6CE8|Note|No data)", raw, ignore.case = TRUE)) {
    return(list(raw = raw, value = NA_real_))
  }
  .nbs_strict_number(raw, context)
}

.nbs_parse_industrial_panel <- function(tables, dataset_id) {
  category_codes <- c(
    "total", "mining", "manufacturing", "utilities", "state_holding",
    "share_holding", "foreign_hk_macao_taiwan", "private"
  )
  financial_measures <- c(
    "business_revenue_level", "business_revenue_yoy",
    "operating_cost_level", "operating_cost_yoy",
    "total_profit_level", "total_profit_yoy"
  )
  industry_codes <- if (
    dataset_id == "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY"
  ) {
    dimensions <- nbs_dataset_dimensions(dataset_id, "en")
    dimensions$value[dimensions$dimension == "industry"]
  } else {
    character()
  }
  records <- list()
  add_table <- function(table_index, header_rows, entity_dimension, measures) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    expected_entities <- if (entity_dimension == "category") {
      category_codes
    } else {
      industry_codes
    }
    labels <- trimws(as.character(table[[1L]]))
    total_row <- which(tolower(labels) == "total")
    if (length(total_row) != 1L) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Industrial table %d did not contain one Total row.", table_index)
      )
    }
    # Older NBS pages repeat one or more visual header rows in the HTML table.
    # Entity rows, however, always begin at the unique Total row and remain
    # contiguous. Anchoring on that semantic row avoids positional shifts while
    # retaining strict cardinality validation for the GB/T4754-2017 panel.
    rows <- seq.int(total_row, nrow(table))
    rows <- rows[!grepl("^\\s*Notes?\\s*:", labels[rows], ignore.case = TRUE)]
    rows <- rows[nzchar(labels[rows])]
    if (length(rows) != length(expected_entities)) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf(
          "Industrial table %d contains %d entities; expected %d.",
          table_index, length(rows), length(expected_entities)
        )
      )
    }
    for (position in seq_along(rows)) {
      row_index <- rows[[position]]
      label <- trimws(table[[1L]][[row_index]])
      entity <- expected_entities[[position]]
      for (column in seq_along(measures) + 1L) {
        number <- .nbs_number_or_published_missing(
          table[[column]][[row_index]],
          sprintf("industrial table %d", table_index)
        )
        records[[length(records) + 1L]] <<- .nbs_panel_record(
          dataset_id, number$raw, number$value, table_index, row_index,
          column, row_label = label,
          category = if (entity_dimension == "category") entity else "",
          industry = if (entity_dimension == "industry") entity else "",
          measure = measures[[column - 1L]],
          period_basis = if (measures[[column - 1L]] %in% c(
            "revenue_per_100_assets", "revenue_per_person",
            "asset_liability_ratio", "finished_goods_turnover_days",
            "receivables_collection_days"
          )) "end_period" else "ytd",
          unit = if (grepl(
            "yoy|margin|ratio", measures[[column - 1L]]
          )) "Percent" else if (grepl("days", measures[[column - 1L]])) {
            "Days"
          } else if (identical(
            measures[[column - 1L]], "revenue_per_person"
          )) {
            "10,000 yuan per person"
          } else if (grepl("per_100|per_100_assets",
                           measures[[column - 1L]])) {
            "Yuan"
          } else {
            "100 million yuan"
          }
        )
      }
    }
  }
  if (dataset_id == "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE") {
    add_table(1L, 2L, "category", financial_measures)
    add_table(
      2L, 3L, "category",
      c(
        "profit_margin", "cost_per_100_revenue",
        "expenses_per_100_revenue", "revenue_per_100_assets",
        "revenue_per_person", "asset_liability_ratio",
        "finished_goods_turnover_days", "receivables_collection_days"
      )
    )
  } else {
    add_table(3L, 3L, "industry", financial_measures)
  }
  do.call(rbind, records)
}

.nbs_parse_housing_panel <- function(
    tables,
    dataset_id,
    index_base = "2025") {
  records <- list()
  city_members <- nbs_dataset_dimensions(dataset_id, "en")
  city_members <- city_members[
    city_members$dimension == "city_code", , drop = FALSE
  ]
  if (nrow(city_members) != 70L ||
      anyDuplicated(city_members$value) ||
      anyDuplicated(city_members$label_en)) {
    .nbs_abort(
      "nbs_manifest_error",
      "The bundled housing city-code master must contain 70 unique cities."
    )
  }
  # The pre-2019 archive is Chinese-only and its city cells contain a mix of
  # ordinary and full-width spaces (and, occasionally, an asterisk footnote).
  # Keep these aliases here rather than changing the public city dimension:
  # the latter remains the stable current bilingual code master.
  city_aliases <- c(
    Xiangfan = "Xiangyang",
    "\u5317\u4EAC" = "Beijing", "\u5929\u6D25" = "Tianjin", "\u77F3\u5BB6\u5E84" = "Shijiazhuang",
    "\u592A\u539F" = "Taiyuan", "\u547C\u548C\u6D69\u7279" = "Hohhot", "\u6C88\u9633" = "Shenyang",
    "\u5927\u8FDE" = "Dalian", "\u957F\u6625" = "Changchun", "\u54C8\u5C14\u6EE8" = "Harbin",
    "\u4E0A\u6D77" = "Shanghai", "\u5357\u4EAC" = "Nanjing", "\u676D\u5DDE" = "Hangzhou",
    "\u5B81\u6CE2" = "Ningbo", "\u5408\u80A5" = "Hefei", "\u798F\u5DDE" = "Fuzhou",
    "\u53A6\u95E8" = "Xiamen", "\u5357\u660C" = "Nanchang", "\u6D4E\u5357" = "Jinan",
    "\u9752\u5C9B" = "Qingdao", "\u90D1\u5DDE" = "Zhengzhou", "\u6B66\u6C49" = "Wuhan",
    "\u957F\u6C99" = "Changsha", "\u5E7F\u5DDE" = "Guangzhou", "\u6DF1\u5733" = "Shenzhen",
    "\u5357\u5B81" = "Nanning", "\u6D77\u53E3" = "Haikou", "\u91CD\u5E86" = "Chongqing",
    "\u6210\u90FD" = "Chengdu", "\u8D35\u9633" = "Guiyang", "\u6606\u660E" = "Kunming",
    "\u897F\u5B89" = "Xi\u2019an", "\u5170\u5DDE" = "Lanzhou", "\u897F\u5B81" = "Xining",
    "\u94F6\u5DDD" = "Yinchuan", "\u4E4C\u9C81\u6728\u9F50" = "Urumqi", "\u5510\u5C71" = "Tangshan",
    "\u79E6\u7687\u5C9B" = "Qinhuangdao", "\u5305\u5934" = "Baotou", "\u4E39\u4E1C" = "Dandong",
    "\u9526\u5DDE" = "Jinzhou", "\u5409\u6797" = "Jilin", "\u7261\u4E39\u6C5F" = "Mudanjiang",
    "\u65E0\u9521" = "Wuxi", "\u5F90\u5DDE" = "Xuzhou", "\u626C\u5DDE" = "Yangzhou",
    "\u6E29\u5DDE" = "Wenzhou", "\u91D1\u534E" = "Jinhua", "\u868C\u57E0" = "Bengbu",
    "\u5B89\u5E86" = "Anqing", "\u6CC9\u5DDE" = "Quanzhou", "\u4E5D\u6C5F" = "Jiujiang",
    "\u8D63\u5DDE" = "Ganzhou", "\u70DF\u53F0" = "Yantai", "\u6D4E\u5B81" = "Jining",
    "\u6D1B\u9633" = "Luoyang", "\u5E73\u9876\u5C71" = "Pingdingshan", "\u5B9C\u660C" = "Yichang",
    "\u8944\u9633" = "Xiangyang", "\u8944\u6A0A" = "Xiangyang", "\u5CB3\u9633" = "Yueyang", "\u5E38\u5FB7" = "Changde",
    "\u97F6\u5173" = "Shaoguan", "\u6E5B\u6C5F" = "Zhanjiang", "\u60E0\u5DDE" = "Huizhou",
    "\u6842\u6797" = "Guilin", "\u5317\u6D77" = "Beihai", "\u4E09\u4E9A" = "Sanya",
    "\u6CF8\u5DDE" = "Luzhou", "\u5357\u5145" = "Nanchong", "\u9075\u4E49" = "Zunyi",
    "\u5927\u7406" = "Dali"
  )
  canonical_city <- function(label) {
    label <- gsub("[[:space:]\u3000\u00A0]+", "", trimws(as.character(label)))
    label <- gsub("[\uFF0A*\u2020\u2021]+$", "", label, perl = TRUE)
    if (label %in% names(city_aliases)) city_aliases[[label]] else label
  }
  city_code <- function(label) {
    label <- canonical_city(label)
    match <- city_members$value[
      city_members$label_en == label | city_members$label_zh == label
    ]
    if (length(match) != 1L) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Housing city `%s` did not map to one official code.", label)
      )
    }
    match[[1L]]
  }
  all_header_text <- paste(
    unlist(lapply(tables[seq_len(min(4L, length(tables)))], function(table) {
      unlist(as.data.frame(table, stringsAsFactors = FALSE), use.names = FALSE)
    })),
    collapse = " "
  )
  detected_base <- regmatches(
    all_header_text,
    regexec("Year[[:space:]]+(2010|2015|2020|2025)[[:space:]]*=[[:space:]]*100", all_header_text,
      ignore.case = TRUE
    )
  )[[1L]]
  if (length(detected_base) == 2L) index_base <- detected_base[[2L]]
  comparison_codes <- function(table, count) {
    header <- paste(
      unlist(as.data.frame(table, stringsAsFactors = FALSE), use.names = FALSE),
      collapse = " "
    )
    if (count == 2L) return(c("mom", "yoy"))
    c(
      "mom", "yoy",
      if (grepl("fixed base|Year[[:space:]]+[0-9]{4}[[:space:]]*=|\u5B9A\u57FA", header,
        ignore.case = TRUE
      )) "fixed_base" else "ytd_avg_yoy"
    )
  }
  published_city_rows <- function(table, columns, expected) {
    labels <- unlist(lapply(columns, function(column) {
      trimws(as.character(table[[column]]))
    }), use.names = FALSE)
    row_ids <- rep(seq_len(nrow(table)), times = length(columns))
    labels <- vapply(labels, canonical_city, character(1L))
    known <- labels %in% c(city_members$label_en, city_members$label_zh)
    rows <- unique(row_ids[known])
    if (length(rows) != expected) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf(
          "A housing table contained %d published city rows; expected %d.",
          length(rows), expected
        )
      )
    }
    rows
  }
  # Some 2011--18 releases place a legacy all-new-housing table before the
  # compatible commercial-residential table. Select tables by their published
  # market heading, never by raw ordinal position, so that legacy concept is
  # not silently stitched into this dataset.
  table_header <- function(table) {
    paste(unlist(as.data.frame(table, stringsAsFactors = FALSE)), collapse = " ")
  }
  headers <- vapply(tables, table_header, character(1L))
  overall_candidates <- which(vapply(tables, ncol, integer(1L)) >= 7L &
    vapply(tables, ncol, integer(1L)) <= 8L)
  new_overall <- overall_candidates[grepl(
    "\u65B0\u5EFA\u5546\u54C1\u4F4F\u5B85|newly built commercial", headers[overall_candidates],
    ignore.case = TRUE
  )]
  second_overall <- overall_candidates[grepl(
    "\u4E8C\u624B\u4F4F\u5B85|second[- ]hand", headers[overall_candidates], ignore.case = TRUE
  )]
  # In several 2018 pages the first (new commercial residential) table has a
  # generic heading while the second-hand table retains its title in-cell.
  # Select its immediately preceding same-shape table, before considering the
  # fully unlabelled fallback below.
  if (!length(new_overall) && length(second_overall)) {
    preceding <- overall_candidates[overall_candidates < second_overall[[1L]]]
    if (length(preceding)) new_overall <- utils::tail(preceding, 1L)
  }
  # The recent English markup and deterministic test fixture omit market names
  # from the table itself; in that unambiguous four-table layout retain the
  # established order. Chinese legacy pages do label the market, so cannot
  # accidentally take this fallback while a pre-break all-new table is present.
  if (!length(new_overall) && !length(second_overall) &&
      length(overall_candidates) == 2L && length(tables) == 4L) {
    new_overall <- overall_candidates[[1L]]
    second_overall <- overall_candidates[[2L]]
  }
  if (!length(new_overall) || !length(second_overall)) {
    .nbs_abort(
      "nbs_press_schema_error",
      "Housing release did not contain compatible commercial-new and second-hand headline tables."
    )
  }
  overall_indices <- c(new_overall[[1L]], second_overall[[1L]])
  # Overall tables contain two 35-city blocks side by side.
  for (market_index in seq_along(overall_indices)) {
    table_index <- overall_indices[[market_index]]
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    market <- if (market_index == 1L) "new" else "second_hand"
    comparison_count <- ncol(table) %/% 2L - 1L
    if (!comparison_count %in% 2:3) {
      .nbs_abort(
        "nbs_press_schema_error",
        "A housing overall table did not contain two or three comparisons."
      )
    }
    comparisons <- comparison_codes(table, comparison_count)
    city_columns <- c(1L, comparison_count + 2L)
    city_rows <- published_city_rows(table, city_columns, 35L)
    for (row_index in city_rows) {
      for (block in 0:1) {
        city_column <- if (block == 0L) 1L else comparison_count + 2L
        city <- trimws(table[[city_column]][[row_index]])
        for (offset in seq_len(comparison_count)) {
          column <- city_column + offset
          number <- .nbs_number_or_published_missing(
            table[[column]][[row_index]], "housing overall table"
          )
          records[[length(records) + 1L]] <- .nbs_panel_record(
            dataset_id, number$raw, number$value, table_index, row_index,
            column, row_label = city, city_code = city_code(city),
            city_label = city, market = market, size_band = "all",
            comparison = comparisons[[offset]], index_base = index_base,
            measure = "index", period_basis = "current", unit = "Index"
          )
        }
      }
    }
  }
  sizes <- c("le90", "90_144", "gt144")
  size_candidates <- which(vapply(tables, ncol, integer(1L)) >= 7L &
    grepl("90|\u5E73\u65B9\u7C73|square", headers, ignore.case = TRUE))
  # The category tables follow the selected headline tables in every official
  # release. This also excludes a possible pre-method-break all-new table.
  size_candidates <- size_candidates[size_candidates > max(overall_indices)]
  if (!length(size_candidates) && length(tables) == 4L) {
    size_candidates <- 3:4
  }
  if (!length(size_candidates)) {
    # The first-release 2011 tables publish only the compatible city headline
    # indices. Preserve those observations with size_band = all; do not invent
    # unreported size-band leaves.
    return(do.call(rbind, records))
  }
  if (length(size_candidates) < 2L) {
    .nbs_abort(
      "nbs_press_schema_error",
      "Housing release did not contain compatible commercial-new and second-hand size-band tables."
    )
  }
  first_size_table <- as.data.frame(
    tables[[size_candidates[[1L]]]], stringsAsFactors = FALSE
  )
  first_size_rows <- published_city_rows(first_size_table, 1L,
    if (nrow(first_size_table) >= 70L) 70L else 35L
  )
  size_table_groups <- if (length(first_size_rows) == 35L) {
    if (length(size_candidates) < 4L) {
      .nbs_abort(
        "nbs_press_schema_error",
        "Housing size-band tables split city blocks without both markets."
      )
    }
    list(size_candidates[1:2], size_candidates[3:4])
  } else {
    list(size_candidates[[1L]], size_candidates[[2L]])
  }
  for (market_index in 1:2) {
    market <- if (market_index == 1L) "new" else "second_hand"
    for (table_index in size_table_groups[[market_index]]) {
      table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
      comparison_count <- (ncol(table) - 1L) %/% 3L
      if (!comparison_count %in% 2:3) {
        .nbs_abort(
          "nbs_press_schema_error",
          "A housing size table did not contain two or three comparisons."
        )
      }
      comparisons <- comparison_codes(table, comparison_count)
      city_rows <- published_city_rows(table, 1L,
        if (nrow(table) >= 70L) 70L else 35L
      )
      for (row_index in city_rows) {
        city <- trimws(table[[1L]][[row_index]])
        for (size_index in seq_along(sizes)) {
          for (comparison_index in seq_len(comparison_count)) {
            column <- 1L + (size_index - 1L) * comparison_count + comparison_index
            number <- .nbs_number_or_published_missing(
              table[[column]][[row_index]], "housing size table"
            )
            records[[length(records) + 1L]] <- .nbs_panel_record(
              dataset_id, number$raw, number$value, table_index, row_index,
              column, row_label = city, city_code = city_code(city),
              city_label = city, market = market,
              size_band = sizes[[size_index]],
              comparison = comparisons[[comparison_index]],
              index_base = index_base, measure = "index",
              period_basis = "current", unit = "Index"
            )
          }
        }
      }
    }
  }
  do.call(rbind, records)
}

.nbs_parse_property_regional_panel <- function(tables, dataset_id) {
  records <- list()
  specifications <- list(
    list(
      table = 2L, header = 2L,
      measures = c(
        "investment_total_level", "investment_residential_level",
        "investment_total_yoy", "investment_residential_yoy"
      ),
      units = c("100 million yuan", "100 million yuan", "Percent", "Percent")
    ),
    list(
      table = 3L, header = 3L,
      measures = c(
        "sales_area_level", "sales_area_yoy",
        "sales_value_level", "sales_value_yoy"
      ),
      units = c(
        "10,000 square metres", "Percent", "100 million yuan", "Percent"
      )
    )
  )
  for (specification in specifications) {
    table <- as.data.frame(
      tables[[specification$table]], stringsAsFactors = FALSE
    )
    rows <- seq.int(specification$header + 1L, nrow(table))
    rows <- rows[tolower(trimws(table[[1L]][rows])) %in% c(
      "eastern", "central", "western", "northeastern"
    )]
    legacy_three_region <- length(rows) == 3L &&
      !any(tolower(trimws(table[[1L]][rows])) == "northeastern")
    for (row_index in rows) {
      label <- trimws(table[[1L]][[row_index]])
      region <- switch(
        tolower(label),
        eastern = if (legacy_three_region) "east_legacy_three_region" else "east",
        central = if (legacy_three_region) {
          "central_legacy_three_region"
        } else {
          "central"
        },
        western = if (legacy_three_region) "west_legacy_three_region" else "west",
        northeastern = "northeast",
        .nbs_press_code(label)
      )
      for (column in 2:5) {
        number <- .nbs_number_or_published_missing(
          table[[column]][[row_index]], "regional property table"
        )
        records[[length(records) + 1L]] <- .nbs_panel_record(
          dataset_id, number$raw, number$value, specification$table,
          row_index, column, row_label = label, region = region,
          measure = specification$measures[[column - 1L]],
          period_basis = "ytd",
          unit = specification$units[[column - 1L]]
        )
      }
    }
  }
  if (!length(records)) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The property release contained no recognized regional aggregate rows."
    )
  }
  do.call(rbind, records)
}

.nbs_parse_property_national_supplement <- function(
    tables,
    document,
    dataset_id) {
  table <- as.data.frame(tables[[1L]], stringsAsFactors = FALSE)
  definitions <- list(
    list(
      label = "Floor space of commercial buildings for sale (10,000 sq.m)",
      pattern = paste0(
        "^Floor Space of (?:The )?Commercial (?:Buildings|Housing) for Sale ",
        "\\(10,000 (?:sq\\.m|square meters)\\)$"
      ),
      ids = c(
        "NBS/CN/M/PROPERTY/FOR_SALE_STOCK/END_LEVEL",
        "NBS/CN/M/PROPERTY/FOR_SALE_STOCK/YOY_PERCENT"
      ),
      units = c("10,000 square metres", "Percent")
    ),
    list(
      label = "Deposits and advance receipts",
      pattern = paste0(
        "^(?:Of Which: )?Deposits and ",
        "(?:Advance Receipts|Advance Payments|Advantage Payments)$"
      ),
      ids = c(
        "NBS/CN/M/PROPERTY/FUNDING/DEPOSITS_ADVANCE_RECEIPTS/YTD_LEVEL",
        "NBS/CN/M/PROPERTY/FUNDING/DEPOSITS_ADVANCE_RECEIPTS/YTD_YOY_PERCENT"
      ),
      units = c("100 million yuan", "Percent")
    ),
    list(
      label = "Individual mortgage loans",
      pattern = "^(?:Individual|Personal) Mortgage(?: Loans?)?$",
      ids = c(
        "NBS/CN/M/PROPERTY/FUNDING/INDIVIDUAL_MORTGAGES/YTD_LEVEL",
        "NBS/CN/M/PROPERTY/FUNDING/INDIVIDUAL_MORTGAGES/YTD_YOY_PERCENT"
      ),
      units = c("100 million yuan", "Percent")
    )
  )
  records <- list()
  normalized_labels <- trimws(gsub(
    "[[:space:]\u00a0]+", " ", as.character(table[[1L]]), perl = TRUE
  ))
  for (definition in definitions) {
    row_index <- which(grepl(
      definition$pattern, normalized_labels,
      ignore.case = TRUE, perl = TRUE
    ))
    if (!length(row_index)) next
    if (length(row_index) > 1L) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Property supplement row `%s` was not unique.", definition$label)
      )
    }
    for (column in 2:3) {
      number <- .nbs_number_or_published_missing(
        table[[column]][[row_index]], "property supplement table"
      )
      series_id <- definition$ids[[column - 1L]]
      records[[length(records) + 1L]] <- .nbs_panel_record(
        dataset_id, number$raw, number$value, 1L, row_index, column,
        row_label = definition$label, measure = series_id,
        period_basis = if (grepl("/YTD_", series_id)) "ytd" else "end_period",
        unit = definition$units[[column - 1L]]
      )
      records[[length(records)]]$series_id <- series_id
    }
  }
  text <- rvest::html_text2(rvest::html_element(document, "body"))
  match <- regmatches(
    text,
    regexec(
      paste0(
        "floor space for sale held for less than three years was ",
        "([0-9.]+) million square meters"
      ),
      text,
      ignore.case = TRUE
    )
  )[[1L]]
  if (length(match) != 2L) {
    if (!length(records)) {
      .nbs_abort(
        "nbs_press_schema_error",
        "The property release contained none of the versioned supplement rows."
      )
    }
    return(do.call(rbind, records))
  }
  series_id <-
    "NBS/CN/M/PROPERTY/FOR_SALE_UNDER_3_YEARS/END_LEVEL"
  extra <- .nbs_panel_record(
    dataset_id, match[[2L]], as.numeric(match[[2L]]) * 100, 0L, 1L, 1L,
    row_label = "For-sale stock held for less than three years",
    measure = series_id, period_basis = "end_period",
    unit = "10,000 square metres"
  )
  extra$series_id <- series_id
  records[[length(records) + 1L]] <- extra
  if (!length(records)) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The property release contained none of the versioned supplement rows."
    )
  }
  do.call(rbind, records)
}

.nbs_parse_retail_panel <- function(tables, dataset_id) {
  records <- list()
  category_members <- nbs_dataset_dimensions(dataset_id, "en")
  category_codes <- category_members$value[
    category_members$dimension == "category_code"
  ]
  category_from_label <- function(label) {
    chinese_label <- gsub(
      "[[:space:]\u3000]+", "", trimws(label), perl = TRUE
    )
    chinese_codes <- c(
      "\u793E\u4F1A\u6D88\u8D39\u54C1\u96F6\u552E\u603B\u989D" = "total_retail_sales_of_consumer_goods",
      "\u5176\u4E2D\uFF1A\u9664\u6C7D\u8F66\u4EE5\u5916\u7684\u6D88\u8D39\u54C1\u96F6\u552E\u989D" = "of_which_retail_sales_of_consumer_goods_excluding_automobiles",
      "\u5176\u4E2D:\u9664\u6C7D\u8F66\u4EE5\u5916\u7684\u6D88\u8D39\u54C1\u96F6\u552E\u989D" = "of_which_retail_sales_of_consumer_goods_excluding_automobiles",
      "\u5176\u4E2D\uFF1A\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u6D88\u8D39\u54C1\u96F6\u552E\u989D" = "of_which_retail_sales_of_enterprises_units_above_the_designated_size",
      "\u5176\u4E2D:\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u6D88\u8D39\u54C1\u96F6\u552E\u989D" = "of_which_retail_sales_of_enterprises_units_above_the_designated_size",
      "\u5176\u4E2D\uFF1A\u5B9E\u7269\u5546\u54C1\u7F51\u4E0A\u96F6\u552E\u989D" = "of_which_online_retail_sales_of_goods",
      "\u5176\u4E2D:\u5B9E\u7269\u5546\u54C1\u7F51\u4E0A\u96F6\u552E\u989D" = "of_which_online_retail_sales_of_goods",
      "\u57CE\u9547" = "urban",
      "\u4E61\u6751" = "rural",
      "\u9910\u996E\u6536\u5165" = "income_of_the_catering_industry",
      "\u5176\u4E2D\uFF1A\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u9910\u996E\u6536\u5165" = "of_which_income_of_the_catering_industry_by_enterprises_units_above_the_designated_size",
      "\u5176\u4E2D:\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u9910\u996E\u6536\u5165" = "of_which_income_of_the_catering_industry_by_enterprises_units_above_the_designated_size",
      "\u5546\u54C1\u96F6\u552E" = "retail_sales_of_goods",
      "\u5176\u4E2D\uFF1A\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u5546\u54C1\u96F6\u552E" = "of_which_retail_sales_of_goods_by_enterprises_units_above_the_designated_size",
      "\u5176\u4E2D:\u9650\u989D\u4EE5\u4E0A\u5355\u4F4D\u5546\u54C1\u96F6\u552E" = "of_which_retail_sales_of_goods_by_enterprises_units_above_the_designated_size",
      "\u7CAE\u6CB9\u3001\u98DF\u54C1\u7C7B" = "of_which_grain_oil_and_food",
      "\u996E\u6599\u7C7B" = "beverage",
      "\u70DF\u9152\u7C7B" = "tobacco_and_liquor",
      "\u670D\u88C5\u3001\u978B\u5E3D\u3001\u9488\u7EBA\u7EC7\u54C1\u7C7B" = "clothes_shoes_hats_and_textiles",
      "\u5316\u5986\u54C1\u7C7B" = "cosmetics",
      "\u91D1\u94F6\u73E0\u5B9D\u7C7B" = "gold_silver_and_jewelry",
      "\u65E5\u7528\u54C1\u7C7B" = "daily_necessities",
      "\u4F53\u80B2\u3001\u5A31\u4E50\u7528\u54C1\u7C7B" = "sports_recreational_articles",
      "\u5BB6\u7528\u7535\u5668\u548C\u97F3\u50CF\u5668\u6750\u7C7B" = "household_appliances_and_audio_video_equipment",
      "\u4E2D\u897F\u836F\u54C1\u7C7B" = "traditional_chinese_and_western_medicines",
      "\u6587\u5316\u529E\u516C\u7528\u54C1\u7C7B" = "cultural_and_office_appliances",
      "\u5BB6\u5177\u7C7B" = "furniture",
      "\u901A\u8BAF\u5668\u6750\u7C7B" = "telecommunication_equipment",
      "\u77F3\u6CB9\u53CA\u5236\u54C1\u7C7B" = "petroleum_and_petroleum_products",
      "\u6C7D\u8F66\u7C7B" = "motor_vehicles",
      "\u5EFA\u7B51\u53CA\u88C5\u6F62\u6750\u6599\u7C7B" = "building_and_decoration_materials"
    )
    if (chinese_label %in% names(chinese_codes)) {
      return(unname(chinese_codes[[chinese_label]]))
    }
    normalized <- tolower(gsub(
      "[^[:alnum:]]+", " ", trimws(label), perl = TRUE
    ))
    normalized <- trimws(gsub("[[:space:]]+", " ", normalized))
    rules <- list(
      of_which_retail_sales_of_consumer_goods_excluding_automobiles =
        "excluding (?:automobile|motor vehicle)",
      of_which_online_retail_sales_of_goods =
        "online retail sales of (?:physical )?(?:goods|commodities)",
      of_which_retail_sales_of_enterprises_units_above_the_designated_size =
        "retail sales (?:of (?:the )?|by )enterprises units above (?:the )?designated size",
      of_which_income_of_the_catering_industry_by_enterprises_units_above_the_designated_size =
        "(?:income of )?catering (?:services|industry) (?:of|by) (?:the )?enterprises units above (?:the )?designated size",
      of_which_retail_sales_of_goods_by_enterprises_units_above_the_designated_size =
        "(?:income of )?retail sales of (?:goods|commodities) (?:of|by) (?:the )?enterprises units above (?:the )?designated size",
      total_retail_sales_of_consumer_goods =
        "^total retail sales of consumer goods$",
      urban = "^(?:urban(?: areas?)?|city)$",
      rural = "^(?:rural(?: areas?)?|at and below county level)$",
      income_of_the_catering_industry = "^(?:income of the )?catering (?:services|industry)$",
      retail_sales_of_goods = "^retail sales of (?:goods|commodities)$",
      of_which_grain_oil_and_food = "grain oil (?:and )?(?:food|foodstuff)",
      beverage = "^beverages?$",
      tobacco_and_liquor = "^tobacco and (?:liquor|alcohol)$",
      clothes_shoes_hats_and_textiles = "clothes shoes hats and textiles",
      cosmetics = "^cosmetics$",
      gold_silver_and_jewelry = "^gold silver and jewelry$",
      daily_necessities = "^daily necessities$",
      sports_recreational_articles =
        "^sports (?:and )?(?:recreational articles|recreation goods|amusements appliances)$",
      household_appliances_and_audio_video_equipment =
        "^household appliances and (?:audio video|av) equipment$",
      traditional_chinese_and_western_medicines =
        "^traditional chinese and western medicines$",
      cultural_and_office_appliances = "^cultural and office (?:appliances|goods)$",
      furniture = "^furniture$",
      telecommunication_equipment =
        "^(?:telecommunication equipment|communication appliances)$",
      petroleum_and_petroleum_products =
        "^petroleum and (?:petroleum|related) products$",
      motor_vehicles = "^(?:motor vehicles|automobiles?)$",
      building_and_decoration_materials =
        "^building and decoration materials$"
    )
    matches <- names(rules)[vapply(
      rules, function(pattern) grepl(pattern, normalized, perl = TRUE),
      logical(1L)
    )]
    if (length(matches) == 1L) matches[[1L]] else ""
  }
  table <- as.data.frame(tables[[1L]], stringsAsFactors = FALSE)
  labels <- trimws(gsub(
    "[[:space:]\u00a0]+", " ", as.character(table[[1L]]), perl = TRUE
  ))
  total_row <- which(
    tolower(labels) == "total retail sales of consumer goods" |
      gsub("[[:space:]\u3000]+", "", labels, perl = TRUE) ==
        "\u793E\u4F1A\u6D88\u8D39\u54C1\u96F6\u552E\u603B\u989D"
  )
  if (length(total_row) != 1L) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The retail headline table did not contain one published total row."
    )
  }
  header_rows <- seq_len(total_row - 1L)
  active_columns <- which(vapply(seq.int(2L, ncol(table)), function(column) {
    values <- trimws(as.character(table[header_rows, column, drop = TRUE]))
    total_value <- gsub(",", "", trimws(as.character(
      table[[column]][[total_row]]
    )), fixed = TRUE)
    # The first row is a visually merged title. Historical NBS HTML sometimes
    # copies the last value into trailing cells whose actual headers are blank.
    any(nzchar(values[-1L])) && grepl(
      "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", total_value
    )
  }, logical(1L))) + 1L
  if (length(active_columns) > 1L) {
    keep <- rep(TRUE, length(active_columns))
    for (position in seq.int(2L, length(active_columns))) {
      current <- trimws(as.character(table[
        total_row:nrow(table), active_columns[[position]], drop = TRUE
      ]))
      prior <- trimws(as.character(table[
        total_row:nrow(table), active_columns[[position - 1L]], drop = TRUE
      ]))
      if (identical(current, prior)) keep[[position]] <- FALSE
    }
    active_columns <- active_columns[keep]
  }
  if (length(active_columns) == 2L) {
    measures <- c("level", "yoy_percent")
    bases <- c("ytd", "ytd")
    units <- c("100 million yuan", "Percent")
  } else if (length(active_columns) == 4L) {
    measures <- c("level", "yoy_percent", "level", "yoy_percent")
    bases <- c("current", "current", "ytd", "ytd")
    units <- c("100 million yuan", "Percent", "100 million yuan", "Percent")
  } else {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf(
        "The retail headline table contained %d active value columns; expected 2 or 4.",
        length(active_columns)
      )
    )
  }
  seen_categories <- character()
  for (row_index in seq.int(total_row, nrow(table))) {
    label <- trimws(table[[1L]][[row_index]])
    category_code <- category_from_label(label)
    if (!nzchar(category_code)) next
    if (!category_code %in% category_codes || category_code %in% seen_categories) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Retail category `%s` did not map uniquely.", label)
      )
    }
    seen_categories <- c(seen_categories, category_code)
    for (position in seq_along(active_columns)) {
      column <- active_columns[[position]]
      raw <- trimws(table[[column]][[row_index]])
      if (raw %in% c("", "-", "--", "\u2026")) {
        next
      }
      number <- .nbs_strict_number(raw, "retail release table")
      records[[length(records) + 1L]] <- .nbs_panel_record(
        dataset_id, number$raw, number$value, 1L, row_index, column,
        row_label = label, category = category_code,
        measure = measures[[position]], period_basis = bases[[position]],
        unit = units[[position]]
      )
    }
  }
  if (length(seen_categories) < 20L) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf(
        "The retail table matched only %d versioned categories.",
        length(seen_categories)
      )
    )
  }
  revision <- as.data.frame(tables[[2L]], stringsAsFactors = FALSE)
  current_year <- NA_integer_
  for (row_index in 2:nrow(revision)) {
    first <- trimws(revision[[1L]][[row_index]])
    if (ncol(revision) == 2L) {
      match_date <- regmatches(
        first,
        regexec("^([0-9]{4})[- ]([A-Za-z]+)$", first)
      )[[1L]]
      if (length(match_date) == 3L) {
        current_year <- as.integer(match_date[[2L]])
        month_text <- match_date[[3L]]
      } else {
        month_text <- first
      }
      value_column <- 2L
    } else {
      year_match <- regmatches(
        first, regexec("^([0-9]{4})(?:\u5E74)?$", first, perl = TRUE)
      )[[1L]]
      if (length(year_match) == 2L) {
        current_year <- as.integer(year_match[[2L]])
      }
      month_text <- trimws(revision[[2L]][[row_index]])
      value_column <- 3L
    }
    chinese_month <- regmatches(
      month_text, regexec("^([0-9]{1,2})\u6708$", month_text, perl = TRUE)
    )[[1L]]
    month <- if (length(chinese_month) == 2L) {
      as.integer(chinese_month[[2L]])
    } else {
      match(tolower(month_text), tolower(month.name))
    }
    if (is.na(current_year) || is.na(month)) {
      next
    }
    number <- .nbs_strict_number(
      revision[[value_column]][[row_index]], "retail SA revision table"
    )
    date <- as.Date(sprintf("%04d-%02d-01", current_year, month))
    records[[length(records) + 1L]] <- .nbs_panel_record(
      dataset_id, number$raw, number$value, 2L, row_index, value_column,
      row_label = "Total retail sales of consumer goods",
      category = "total_retail_sales_of_consumer_goods",
      measure = "mom_percent_official_sa",
      period_basis = "current", unit = "Percent", observation_date = date
    )
  }
  do.call(rbind, records)
}

.nbs_parse_household_real_panel <- function(tables, dataset_id) {
  table <- as.data.frame(tables[[1L]], stringsAsFactors = FALSE)
  labels <- trimws(as.character(table[[1L]]))
  labels_normalized <- trimws(gsub(
    "[[:space:]\u00a0]+", " ", labels, perl = TRUE
  ))
  income_row <- which(
    (
      grepl("per capita", labels_normalized, ignore.case = TRUE) &
        grepl("income", labels_normalized, ignore.case = TRUE) &
        !grepl("median", labels_normalized, ignore.case = TRUE) &
        grepl("nationwide|national|households", labels_normalized,
          ignore.case = TRUE
        )
    ) |
      (
        grepl("\u5168\u56FD\u5C45\u6C11\u4EBA\u5747\u53EF\u652F\u914D\u6536\u5165", labels_normalized, fixed = TRUE) &
          !grepl("\u4E2D\u4F4D\u6570", labels_normalized, fixed = TRUE)
      )
  )
  expenditure_row <- which(
    (
      grepl("per capita", labels_normalized, ignore.case = TRUE) &
        grepl("expenditure", labels_normalized, ignore.case = TRUE) &
        grepl("nationwide|national|households", labels_normalized,
          ignore.case = TRUE
        )
    ) |
      grepl("\u5168\u56FD\u5C45\u6C11\u4EBA\u5747\u6D88\u8D39\u652F\u51FA", labels_normalized, fixed = TRUE)
  )
  if (!length(income_row) || !length(expenditure_row)) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The household headline income or expenditure row was not found."
    )
  }
  income_row <- income_row[[1L]]
  expenditure_row <- expenditure_row[[1L]]
  residence_rows_after <- function(row) {
    candidates <- seq.int(row + 1L, nrow(table))
    urban <- candidates[grepl(
      "^(?:urban (?:residents|households)|\u57CE\u9547\u5C45\u6C11)$",
      labels_normalized[candidates],
      ignore.case = TRUE, perl = TRUE
    )]
    rural <- candidates[grepl(
      "^(?:rural (?:residents|households)|\u519C\u6751\u5C45\u6C11)$",
      labels_normalized[candidates],
      ignore.case = TRUE, perl = TRUE
    )]
    if (!length(urban) || !length(rural)) {
      .nbs_abort(
        "nbs_press_schema_error",
        "The household urban or rural headline row was not found."
      )
    }
    c(urban[[1L]], rural[[1L]])
  }
  rows <- c(
    income_row, residence_rows_after(income_row),
    expenditure_row, residence_rows_after(expenditure_row)
  )
  residence <- c("national", "urban", "rural", "national", "urban", "rural")
  measure <- c(
    rep("disposable_income_yoy_percent", 3L),
    rep("consumption_expenditure_yoy_percent", 3L)
  )
  records <- lapply(seq_along(rows), function(index) {
    row_index <- rows[[index]]
    row_values <- trimws(as.character(table[row_index, , drop = TRUE]))
    matches <- lapply(row_values, function(raw) {
      regmatches(
        raw,
        regexec("[\\(\uff08]([+-]?[0-9.]+)[\\)\uff09]", raw)
      )[[1L]]
    })
    real_values <- unique(vapply(
      Filter(function(value) length(value) == 2L, matches),
      `[[`, character(1L), 2L
    ))
    if (length(real_values) != 1L) {
      .nbs_abort(
        "nbs_press_schema_error",
        "A household real-growth value was not present in brackets."
      )
    }
    raw <- real_values[[1L]]
    column_index <- which(vapply(
      matches,
      function(value) length(value) == 2L && identical(value[[2L]], raw),
      logical(1L)
    ))[[1L]]
    record <- .nbs_panel_record(
      dataset_id, raw, as.numeric(raw), 1L, row_index, column_index,
      row_label = trimws(table[[1L]][[row_index]]),
      measure = measure[[index]], period_basis = "ytd",
      residence = residence[[index]], nominal_real = "real",
      unit = "Percent"
    )
    record$series_id <- paste(
      "NBS/CN/Q/HOUSEHOLDS",
      toupper(residence[[index]]),
      if (grepl("^disposable", measure[[index]])) {
        "DISPOSABLE_INCOME"
      } else {
        "CONSUMPTION_EXPENDITURE"
      },
      "YTD_REAL_YOY_PERCENT",
      sep = "/"
    )
    record
  })
  do.call(rbind, records)
}

.nbs_parse_demography_panel <- function(document, dataset_id) {
  text <- rvest::html_text2(rvest::html_element(document, "body"))
  specifications <- list(
    births_level = list(
      c("There were ([0-9.]+) million births", 100),
      c("\u5168\u5E74\u51FA\u751F\u4EBA\u53E3\\s*([0-9.]+)\\s*\u4E07\u4EBA", 1)
    ),
    deaths_level = list(
      c("there were ([0-9.]+) million deaths", 100),
      c("\u6B7B\u4EA1\u4EBA\u53E3\\s*([0-9.]+)\\s*\u4E07\u4EBA", 1)
    ),
    urbanisation_percent = list(
      c("urbanization rate of permanent residents reached ([0-9.]+) percent", 1),
      c("\u5E38\u4F4F\u4EBA\u53E3\u57CE\u9547\u5316\u7387(?:\u4E3A|\u8FBE\u5230)\\s*([0-9.]+)\\s*%", 1),
      c("\u57CE\u9547\u4EBA\u53E3[^\u3002]{0,120}\u5360\u603B\u4EBA\u53E3\u6BD4\u91CD\u4E3A\\s*([0-9.]+)\\s*%", 1)
    ),
    age_15_59_year_end = list(
      c("aged 15 to 59 \\(under the age of 60\\) was ([0-9.]+) million", 100),
      c("15\\s*[-\u2014\uFF0D]\\s*59\u5C81[^\u3002]{0,80}?\u4EBA\u53E3\\s*([0-9.]+)\\s*\u4E07\u4EBA", 1)
    ),
    age_16_59_year_end = list(
      c("aged 16[- ]59 \\(under the age of 60\\)(?: was)? ([0-9.]+) million", 100),
      c("16\\s*[-\u2014\uFF0D]\\s*59\u5C81[^\u3002]{0,80}?\u4EBA\u53E3\\s*([0-9.]+)\\s*\u4E07\u4EBA", 1)
    ),
    age_60_plus_year_end = list(
      c("Aged 60 and above\\s+([0-9]+)", 1),
      c("60\u5468?\u5C81\u53CA\u4EE5\u4E0A\u4EBA\u53E3\\s*([0-9.]+)\\s*\u4E07\u4EBA", 1)
    )
  )
  # Population tables are more stable than prose across Chinese and English
  # communiques. Use their published level/share cells for urbanisation and
  # age bands, including the official 15-59 to 16-59 definition break in 2013.
  tables <- rvest::html_table(document, fill = TRUE)
  table_values <- list()
  for (table in tables) {
    table <- as.data.frame(table, stringsAsFactors = FALSE)
    if (ncol(table) < 3L) next
    labels <- trimws(gsub("[[:space:]\u00a0]+", " ", as.character(table[[1L]])))
    urban <- which(grepl("^(?:Of which: )?Urban$|^\u5176\u4E2D[\uFF1A:]?\u57CE\u9547$", labels,
      ignore.case = TRUE, perl = TRUE
    ))
    age_15 <- which(grepl("^15[-\u2014\uFF0D]59\u5C81", labels, perl = TRUE))
    age_16 <- which(grepl("^(?:Aged )?16[-\u2014\uFF0D]59|^16[-\u2014\uFF0D]59\u5C81", labels,
      ignore.case = TRUE, perl = TRUE
    ))
    age_60 <- which(grepl("^(?:Aged )?60 (?:and above|years? and over)$|^60\u5468?\u5C81\u53CA\u4EE5\u4E0A$",
      labels, ignore.case = TRUE, perl = TRUE
    ))
    if (length(urban) == 1L && length(age_60) == 1L &&
        length(c(age_15, age_16)) == 1L) {
      table_values$urbanisation_percent <- .nbs_strict_number(
        table[[3L]][[urban]], "demography population table"
      )
      age_row <- c(age_15, age_16)[[1L]]
      age_name <- if (length(age_15)) {
        "age_15_59_year_end"
      } else {
        "age_16_59_year_end"
      }
      table_values[[age_name]] <- .nbs_strict_number(
        table[[2L]][[age_row]], "demography population table"
      )
      table_values$age_60_plus_year_end <- .nbs_strict_number(
        table[[2L]][[age_60]], "demography population table"
      )
      break
    }
  }
  if (nzchar(table_values$age_15_59_year_end$raw %||% "")) {
    specifications$age_16_59_year_end <- NULL
  } else if (nzchar(table_values$age_16_59_year_end$raw %||% "")) {
    specifications$age_15_59_year_end <- NULL
  } else {
    prose_matches <- function(candidates) {
      any(vapply(candidates, function(candidate) {
        grepl(candidate[[1L]], text, ignore.case = TRUE, perl = TRUE)
      }, logical(1L)))
    }
    has_age_15 <- prose_matches(specifications$age_15_59_year_end)
    has_age_16 <- prose_matches(specifications$age_16_59_year_end)
    if (has_age_15 && !has_age_16) {
      specifications$age_16_59_year_end <- NULL
    } else if (has_age_16 && !has_age_15) {
      specifications$age_15_59_year_end <- NULL
    }
  }
  records <- lapply(seq_along(specifications), function(index) {
    measure_name <- names(specifications)[index]
    if (!is.null(table_values[[measure_name]])) {
      parsed <- list(raw = table_values[[measure_name]]$raw, factor = 1)
    } else {
    candidates <- specifications[[index]]
    parsed <- NULL
    for (candidate in candidates) {
      match <- regmatches(
        text, regexec(candidate[[1L]], text, ignore.case = TRUE, perl = TRUE)
      )[[1L]]
      if (length(match) == 2L) {
        parsed <- list(raw = match[[2L]], factor = as.numeric(candidate[[2L]]))
        break
      }
    }
    }
    if (is.null(parsed)) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf(
          "Could not locate demographic measure `%s`.",
          measure_name
        )
      )
    }
    value <- as.numeric(parsed$raw) * parsed$factor
    unit <- if (measure_name == "urbanisation_percent") {
      "Percent"
    } else {
      "10,000 persons"
    }
    .nbs_panel_record(
      dataset_id, parsed$raw, value, 0L, index, 1L,
      row_label = measure_name,
      measure = measure_name, period_basis = "annual", unit = unit
    )
  })
  do.call(rbind, records)
}

.nbs_parse_wages_panel <- function(tables, dataset_id) {
  records <- list()
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  dimension_values <- function(name) {
    setdiff(
      dimensions$value[dimensions$dimension == name],
      "not_applicable"
    )
  }
  entity_values <- function(name, unit_type) {
    values <- dimension_values(name)
    if (name == "industry" && unit_type == "private") {
      values <- setdiff(
        values,
        "public_management_social_security_and_social_organizations"
      )
    }
    if (name == "industry" && unit_type == "above_designated") {
      values <- setdiff(values, c(
        "agriculture_forestry_animal_husbandry_and_fishery",
        "finance",
        "public_management_social_security_and_social_organizations"
      ))
    }
    values
  }
  registration_rows <- function(table, entities) {
    labels <- trimws(as.character(table[[1L]]))
    labels <- gsub("[[:space:]\u00a0\u3000]+", " ", labels, perl = TRUE)
    patterns <- list(
      total = "^(?:total|\u5408 *\u8BA1)$",
      domestic_invested_enterprises = paste0(
        "^(?:domestic[- ]invested (?:units|enterprises)|",
        "[\uFF08(]?\u4E00[\uFF09)]?\u5185\u8D44)$"
      ),
      enterprises_with_investment_from_hong_kong_macao_and_taiwan =
        "(?:hong kong.*macao.*taiwan|\u6E2F\u6FB3\u53F0\u6295\u8D44)$",
      foreign_invested_enterprises =
        paste0(
          "^(?:foreign[- ]invested (?:units|enterprises)|",
          "units funded by foreign investors|[\uFF08(]?\u4E09[\uFF09)]?\u5916\u5546\u6295\u8D44)$"
        )
    )
    rows <- vapply(entities, function(entity) {
      match_rows <- which(grepl(
        patterns[[entity]], labels, ignore.case = TRUE, perl = TRUE
      ))
      if (length(match_rows) != 1L) {
        .nbs_abort(
          "nbs_press_schema_error",
          sprintf("Annual wage registration `%s` did not resolve uniquely.", entity)
        )
      }
      match_rows[[1L]]
    }, integer(1))
    unname(rows)
  }
  parse_simple <- function(table_index, unit_type, entity_dimension) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    entities <- entity_values(entity_dimension, unit_type)
    rows <- if (entity_dimension == "registration") {
      registration_rows(table, entities)
    } else {
      3:nrow(table)
    }
    if (length(rows) != length(entities)) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf(
          "Annual wage table %d contains %d entities; expected %d.",
          table_index, length(rows), length(entities)
        )
      )
    }
    for (position in seq_along(rows)) {
      row_index <- rows[[position]]
      label <- trimws(table[[1L]][[row_index]])
      measures <- if (ncol(table) == 2L) {
        "current_year_average_wage"
      } else {
        c(
          "current_year_average_wage", "prior_year_average_wage",
          "nominal_growth_rate"
        )
      }
      if (!ncol(table) %in% c(2L, 4L)) {
        .nbs_abort(
          "nbs_press_schema_error",
          sprintf("Annual wage table %d changed measure columns.", table_index)
        )
      }
      for (column in seq_along(measures) + 1L) {
        number <- .nbs_strict_number(
          table[[column]][[row_index]], "annual wage table"
        )
        measure <- measures[[column - 1L]]
        record <- .nbs_panel_record(
          dataset_id, number$raw, number$value, table_index, row_index,
          column, row_label = label, unit_type = unit_type,
          measure = measure, period_basis = "annual",
          unit = if (column == 4L) "Percent" else "Yuan"
        )
        record[[entity_dimension]] <- entities[[position]]
        record$region[entity_dimension != "region"] <- "not_applicable"
        record$industry[entity_dimension != "industry"] <- "not_applicable"
        record$registration[entity_dimension != "registration"] <-
          "not_applicable"
        record$occupation <- "not_applicable"
        records[[length(records) + 1L]] <<- record
      }
    }
  }
  parse_occupation <- function(table_index, entity_dimension) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    occupations <- dimension_values("occupation")
    entities <- entity_values(entity_dimension, "above_designated")
    rows <- if (entity_dimension == "registration") {
      registration_rows(table, entities)
    } else {
      4:nrow(table)
    }
    if (ncol(table) - 1L != length(occupations) ||
        length(rows) != length(entities)) {
      .nbs_abort(
        "nbs_press_schema_error",
        sprintf("Annual occupation wage table %d changed shape.", table_index)
      )
    }
    for (position in seq_along(rows)) {
      row_index <- rows[[position]]
      label <- trimws(table[[1L]][[row_index]])
      for (column in 2:ncol(table)) {
        number <- .nbs_strict_number(
          table[[column]][[row_index]], "annual occupation wage table"
        )
        record <- .nbs_panel_record(
          dataset_id, number$raw, number$value, table_index, row_index,
          column, row_label = label, unit_type = "above_designated",
          measure = "occupation_average_wage", period_basis = "annual",
          occupation = occupations[[column - 1L]], unit = "Yuan"
        )
        record[[entity_dimension]] <- entities[[position]]
        record$region[entity_dimension != "region"] <- "not_applicable"
        record$industry[entity_dimension != "industry"] <- "not_applicable"
        record$registration[entity_dimension != "registration"] <-
          "not_applicable"
        records[[length(records) + 1L]] <<- record
      }
    }
  }
  parse_simple(1L, "non_private", "region")
  parse_simple(2L, "non_private", "industry")
  parse_simple(3L, "non_private", "registration")
  parse_simple(4L, "private", "region")
  parse_simple(5L, "private", "industry")
  parse_occupation(6L, "region")
  parse_occupation(7L, "industry")
  parse_occupation(8L, "registration")
  do.call(rbind, records)
}

.nbs_migrant_measure_metadata <- function(measure) {
  dimensions <- .nbs_load_dataset_dimensions()
  member <- dimensions[
    dimensions$dataset_id == "NBS/CN/A/MIGRANT_WORKERS" &
      dimensions$dimension == "measure" &
      dimensions$value == measure,
    ,
    drop = FALSE
  ]
  if (nrow(member) != 1L ||
      !nzchar(member$label_en[[1L]]) ||
      !nzchar(member$label_zh[[1L]]) ||
      !nzchar(member$unit_en[[1L]]) ||
      !nzchar(member$unit_zh[[1L]])) {
    .nbs_abort(
      "nbs_press_schema_error",
      "A migrant-report cell did not match one exact versioned selector."
    )
  }
  list(
    label_en = member$label_en[[1L]],
    label_zh = member$label_zh[[1L]],
    unit_en = member$unit_en[[1L]],
    unit_zh = member$unit_zh[[1L]]
  )
}

.nbs_parse_migrant_panel <- function(tables, dataset_id) {
  if (length(tables) < 5L) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The migrant-worker report contained fewer than five tables."
    )
  }
  if (length(tables) == 10L) {
    duplicated_pairs <- vapply(seq_len(5L), function(index) {
      identical(
        as.data.frame(tables[[index]], stringsAsFactors = FALSE),
        as.data.frame(tables[[index + 5L]], stringsAsFactors = FALSE)
      )
    }, logical(1))
    if (!all(duplicated_pairs)) {
      .nbs_abort(
        "nbs_press_schema_error",
        "The migrant report's duplicated responsive tables are inconsistent."
      )
    }
  } else if (length(tables) != 5L) {
    .nbs_abort(
      "nbs_press_schema_error",
      sprintf(
        "The migrant report contained %d tables; expected 5 or 10.",
        length(tables)
      )
    )
  }
  records <- list()
  header_rows <- c(4L, 2L, 2L, 2L, 2L)
  for (table_index in seq_len(5L)) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    header <- header_rows[[table_index]]
    headers <- vapply(seq_len(ncol(table)), function(column) {
      values <- trimws(as.character(table[seq_len(header), column]))
      values <- unique(values[nzchar(values)])
      paste(values, collapse = " | ")
    }, character(1))
    for (row_index in seq.int(header + 1L, nrow(table))) {
      row_label <- trimws(table[[1L]][[row_index]])
      for (column in 2:ncol(table)) {
        raw <- trimws(table[[column]][[row_index]])
        normalized <- gsub(",", "", raw, fixed = TRUE)
        if (!grepl(
          "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", normalized
        )) {
          next
        }
        measure <- .nbs_press_code(paste(row_label, headers[[column]]))
        metadata <- .nbs_migrant_measure_metadata(measure)
        record <- .nbs_panel_record(
          dataset_id, raw, as.numeric(normalized), table_index, row_index,
          column, row_label = row_label,
          measure = measure, period_basis = "annual",
          section = paste0("table_", table_index),
          unit = metadata$unit_en
        )
        record$dimension_label_en <- metadata$label_en
        record$dimension_label_zh <- metadata$label_zh
        record$unit_en <- metadata$unit_en
        record$unit_zh <- metadata$unit_zh
      records[[length(records) + 1L]] <- record
    }
  }
  if (!length(records)) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The property release contained none of the versioned supplement rows."
    )
  }
  }
  do.call(rbind, records)
}

.nbs_parse_ten_day_panel <- function(
    tables,
    document,
    dataset_id,
    include_direction_counts = TRUE) {
  price <- as.data.frame(tables[[1L]], stringsAsFactors = FALSE)
  specification <- as.data.frame(tables[[2L]], stringsAsFactors = FALSE)
  specification_rows <- which(grepl(
    "^[0-9]+$", trimws(as.character(specification[[1L]]))
  ))
  records <- list()
  category <- ""
  product_index <- 0L
  for (row_index in 2:(nrow(price) - 1L)) {
    raw_price <- trimws(price[[3L]][[row_index]])
    if (!grepl("^[+-]?[0-9]", raw_price)) {
      label <- trimws(price[[1L]][[row_index]])
      if (nzchar(label)) {
        category <- .nbs_press_code(label)
      }
      next
    }
    product_index <- product_index + 1L
    product_code <- sprintf("p%03d", product_index)
    spec_row <- specification_rows[[product_index]]
    product <- trimws(specification[[2L]][[spec_row]])
    spec <- trimws(specification[[3L]][[spec_row]])
    unit <- trimws(price[[2L]][[row_index]])
    physical_unit <- switch(
      tolower(unit),
      ton = "tonne",
      kg = "kilogram",
      "\u5428" = "tonne",
      "\u5343\u514B" = "kilogram",
      tolower(unit)
    )
    measures <- c("price_yuan", "change_yuan", "change_percent")
    for (column in 3:5) {
      number <- .nbs_strict_number(
        price[[column]][[row_index]], "ten-day price table"
      )
      records[[length(records) + 1L]] <- .nbs_panel_record(
        dataset_id, number$raw, number$value, 1L, row_index, column,
        row_label = product, category = category,
        product_code = product_code, specification = spec,
        measure = measures[[column - 2L]], period_basis = "ten_day",
        unit = if (column == 5L) {
          "Percent"
        } else {
          paste("Yuan per", physical_unit)
        }
      )
    }
  }
  if (product_index != 50L) {
    .nbs_abort(
      "nbs_dataset_cardinality_error",
      sprintf(
        "The ten-day release contained %d product rows; expected 50.",
        product_index
      ),
      expected_rows = 50L,
      actual_rows = product_index
    )
  }
  text <- rvest::html_text2(rvest::html_element(document, "body"))
  match <- regmatches(
    text,
    regexec(
      paste0(
        "prices of ([0-9]+)(?: kinds? of)? products? increased, ",
        "([0-9]+) kinds? decreased, and ([0-9]+) kinds? ",
        "(?:remained flat|kept at the same level|remained unchanged)"
      ),
      text,
      ignore.case = TRUE
    )
  )[[1L]]
  if (length(match) != 4L) {
    match <- regmatches(
      text,
      regexec(
        paste0(
          "([0-9]+)\u79CD\u4EA7\u54C1\u4EF7\u683C\u4E0A\u6DA8[\uFF0C,]\\s*([0-9]+)\u79CD\u4E0B\u964D[\uFF0C,]\\s*",
          "([0-9]+)\u79CD\u6301\u5E73"
        ),
        text,
        perl = TRUE
      )
    )[[1L]]
  }
  if (length(match) != 4L && include_direction_counts) {
    .nbs_abort(
      "nbs_press_schema_error",
      "Ten-day direction counts were not found."
    )
  }
  if (length(match) != 4L) {
    return(do.call(rbind, records))
  }
  direction_measures <- c(
    "increased_product_count", "decreased_product_count",
    "flat_product_count"
  )
  for (index in seq_len(3L)) {
    records[[length(records) + 1L]] <- .nbs_panel_record(
      dataset_id, match[[index + 1L]], as.numeric(match[[index + 1L]]),
      0L, index, 1L, row_label = direction_measures[[index]],
      category = "summary", product_code = "summary",
      specification = "not_applicable",
      measure = direction_measures[[index]], period_basis = "ten_day",
      unit = "Products"
    )
  }
  do.call(rbind, records)
}

.nbs_parse_press_dataset_specific <- function(
    document,
    tables,
    dataset,
    release = NULL) {
  dataset_id <- dataset$dataset_id[[1L]]
  if (dataset_id %in% c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY"
  )) {
    return(.nbs_parse_industrial_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY") {
    index_base <- if (!is.null(release) && nzchar(release$index_base[[1L]])) {
      release$index_base[[1L]]
    } else {
      "2025"
    }
    return(.nbs_parse_housing_panel(tables, dataset_id, index_base))
  }
  if (dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION") {
    return(.nbs_parse_property_regional_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL") {
    return(.nbs_parse_property_national_supplement(
      tables, document, dataset_id
    ))
  }
  if (dataset_id == "NBS/CN/M/RETAIL_SALES/DETAIL") {
    return(.nbs_parse_retail_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION") {
    return(.nbs_parse_household_real_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION") {
    return(.nbs_parse_capacity_utilisation_panel(
      tables, document, dataset_id, release
    ))
  }
  if (dataset_id == "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE") {
    return(.nbs_parse_demography_panel(document, dataset_id))
  }
  if (dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES") {
    return(.nbs_parse_wages_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/A/MIGRANT_WORKERS") {
    return(.nbs_parse_migrant_panel(tables, dataset_id))
  }
  if (dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES") {
    return(.nbs_parse_ten_day_panel(
      tables, document, dataset_id, include_direction_counts = FALSE
    ))
  }
  NULL
}
