#!/usr/bin/env Rscript

# Regenerates the bounded industrial portal supplements from official NBS
# catalogue and indicator metadata.  It deliberately treats every official
# classification / registration branch as a separate dataset: never use the
# output to infer an equivalence between eras.

args <- commandArgs(trailingOnly = TRUE)
verify_existing <- "--verify-existing" %in% args
verify_dataset <- sub("^--verify-dataset=", "", args[
  grepl("^--verify-dataset=", args)
])
if (length(verify_dataset) > 1L) stop("Use at most one --verify-dataset value.")
verify_catalog <- sub("^--verify-catalog=", "", args[
  grepl("^--verify-catalog=", args)
])
if (length(verify_catalog) > 1L) stop("Use at most one --verify-catalog value.")
if (length(verify_catalog)) {
  verify_catalog <- strsplit(verify_catalog, ",", fixed = TRUE)[[1L]]
}
refresh_datasets <- "--refresh-datasets" %in% args
refresh_units <- "--refresh-units" %in% args
drop_unverified <- "--drop-unverified" %in% args
args <- setdiff(args, "--verify-existing")
args <- args[!grepl("^--verify-dataset=", args)]
args <- args[!grepl("^--verify-catalog=", args)]
args <- setdiff(args, "--refresh-datasets")
args <- setdiff(args, "--refresh-units")
args <- setdiff(args, "--drop-unverified")
output_dir <- if (length(args)) args[[1L]] else file.path("inst", "extdata")
if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

source(file.path("R", "conditions.R"))
source(file.path("R", "utils.R"))
source(file.path("R", "http.R"))
source(file.path("R", "parse.R"))
source(file.path("R", "client.R"))
source(file.path("R", "discovery.R"))
source(file.path("R", "periods.R"))

version <- "2026-08-11.1"
root_uuid <- "fc982599aa684be7969d7b90b1bd0e84"

slug <- function(value) {
  value <- iconv(value, to = "ASCII//TRANSLIT")
  value <- tolower(gsub("[^A-Za-z0-9]+", "_", value))
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) stop("Could not make a deterministic ASCII slug.")
  value
}

csv <- function(data, filename) {
  path <- file.path(output_dir, filename)
  connection <- if (grepl("\\.gz$", filename)) gzfile(path, "wt") else path
  on.exit(if (inherits(connection, "connection")) close(connection), add = TRUE)
  utils::write.csv(data, connection, row.names = FALSE, na = "",
    fileEncoding = "UTF-8")
}

# NBS's `du` metadata can be stale on individual industrial leaves (for
# example, some employee series carry the Yuan unit UUID).  The official
# published indicator title is the authoritative, retained unit text.  This
# parser intentionally accepts only units actually present in the titles used
# by these exact UUID panels; a new unrecognised unit must fail closed.
official_unit_from_display_name <- function(display_name) {
  normalized <- tolower(gsub("\\s+", " ", trimws(display_name)))
  if (grepl("%", normalized, fixed = TRUE)) {
    return(c(unit_en = "Percent", unit_zh = "Percent"))
  }
  if (grepl("100 million yuan", normalized, fixed = TRUE)) {
    return(c(unit_en = "100 million yuan", unit_zh = "100 million yuan"))
  }
  if (grepl("10,?000\\s*persons?", normalized, perl = TRUE)) {
    return(c(unit_en = "10,000 persons", unit_zh = "万人"))
  }
  if (grepl("^number of .*enterprises.*\\(unit\\)$", normalized,
      perl = TRUE)) {
    return(c(unit_en = "Enterprises", unit_zh = "家"))
  }
  c(unit_en = NA_character_, unit_zh = NA_character_)
}

derive_official_units <- function(display_names) {
  parsed <- t(vapply(display_names, official_unit_from_display_name,
    character(2)
  ))
  data.frame(unit_en = parsed[, "unit_en"], unit_zh = parsed[, "unit_zh"],
    stringsAsFactors = FALSE)
}

# Four legacy registration-type UUIDs carry a copied English current-assets
# title in the portal even though their Chinese names and position in the
# official three-member groups identify financial-cost or interest-expense
# accumulations. Keep the UUID identity authoritative, but correct the public
# English title and period meaning. The first two members of each group are
# the accumulated value and the same period of the previous year; the third
# member is the already-correct accumulated growth-rate leaf.
legacy_display_name_corrections <- data.frame(
  indicator_uuid = tolower(c(
    "363F3D8F6D5D48AD9F495B99BEA879B8",
    "BDAE32CF49F54822BA09942AF4633CB5",
    "8E5C303DDD2C45B99FDF152344CBC02D",
    "690AEEC30DBD4436A5C7FCE49AE48324"
  )),
  source_name = c(
    "(发布库已用)财务费用(zy)",
    "(发布库已用)财务费用(zy)",
    "(发布库已用)利息支出",
    "(发布库已用)利息支出"
  ),
  period_dimension = c("21", "24", "21", "24"),
  display_name = c(
    "Financial Costs of Collective-owned Industrial Enterprises, Accumulated (100 million yuan)",
    "Financial Costs of Collective-owned Industrial Enterprises, Accumulated, The Same Period of Previous Year (100 million yuan)",
    "Interest Expenses of Collective-owned Industrial Enterprises, Accumulated (100 million yuan)",
    "Interest Expenses of Collective-owned Industrial Enterprises, Accumulated, The Same Period of Previous Year (100 million yuan)"
  ),
  stringsAsFactors = FALSE
)

correct_legacy_display_names <- function(indicators) {
  source_rows <- match(
    legacy_display_name_corrections$indicator_uuid,
    tolower(indicators$indicator_uuid)
  )
  if (anyNA(source_rows)) {
    stop("Official metadata no longer contains every legacy label correction.")
  }
  if (any(
    indicators$name[source_rows] !=
      legacy_display_name_corrections$source_name
  )) {
    stop("Legacy industrial label correction no longer matches source metadata.")
  }
  if (any(
    indicators$period_dimension[source_rows] !=
      legacy_display_name_corrections$period_dimension
  )) {
    stop("Legacy industrial label correction no longer matches period metadata.")
  }
  indicators$display_name[source_rows] <-
    legacy_display_name_corrections$display_name
  indicators
}

configs <- list(
  list(
    dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/THROUGH_2023",
    parent = "1a2fe800929b46508686d5ce3bbc26d5",
    kind = "registration_type", classification = "registration-type-through-2023",
    start = "2010-02-01", end = "2023-12-01",
    title_en = "Industrial financial indicators by registration type (through 2023)",
    title_zh = "按登记注册类型分工业企业财务指标（截至2023年）"
  ),
  list(
    dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_REGISTRATION_TYPE/FROM_2024",
    parent = "60d079366cb04de5a6e14065678c1090",
    kind = "registration_type", classification = "registration-type-2024-present",
    start = "2024-02-01", end = "",
    title_en = "Industrial financial indicators by registration type (from 2024)",
    title_zh = "按登记注册类型分工业企业财务指标（2024年起）"
  ),
  list(
    dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2002",
    parent = "b0c4b413cb394a98a2bef12f19d6d7dc",
    kind = "industry", classification = "gb-t-4754-2002-2003-2011",
    start = "2010-02-01", end = "2011-12-01",
    title_en = "Industrial financial indicators by industry (GB/T 4754-2002; 2003-2011)",
    title_zh = "按行业分工业企业财务指标（GB/T 4754-2002；2003—2011年）"
  ),
  list(
    dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011",
    parent = "2f1dfd86443744f7850f41d6a2776dfd",
    kind = "industry", classification = "gb-t-4754-2011-2012-2017",
    start = "2012-02-01", end = "2017-12-01",
    title_en = "Industrial financial indicators by industry (GB/T 4754-2011; 2012-2017)",
    title_zh = "按行业分工业企业财务指标（GB/T 4754-2011；2012—2017年）"
  ),
  list(
    dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017",
    parent = "1e49af0eeed74047a9ff4271dffba01d",
    kind = "industry", classification = "gb-t-4754-2017-2018-present",
    start = "2018-02-01", end = "",
    title_en = "Industrial financial indicators by industry (GB/T 4754-2017; from 2018)",
    title_zh = "按行业分工业企业财务指标（GB/T 4754-2017；2018年起）"
  )
)

date_from_code <- function(code) {
  match <- regexec("^([0-9]{4})([0-9]{2})MM$", code)
  pieces <- regmatches(code, match)[[1L]]
  if (length(pieces) != 3L) stop("Unexpected NBS monthly period code: ", code)
  as.Date(sprintf("%s-%s-01", pieces[[2L]], pieces[[3L]]))
}

continuous_available_rows <- function(manifest) {
  client <- nbs_client(retries = 3L)
  present <- vector("list", nrow(manifest))
  names(present) <- manifest$series_id
  catalogues <- split(seq_len(nrow(manifest)), manifest$catalog_uuid)
  for (catalog_uuid in names(catalogues)) {
    indexes <- catalogues[[catalog_uuid]]
    # The public endpoint accepts a batch of indicator IDs for one catalogue.
    # Keep chunks comfortably below its undocumented request-size ceiling.
    for (chunk in split(indexes, ceiling(seq_along(indexes) / 160L))) {
      rows <- manifest[chunk, , drop = FALSE]
      start <- min(as.Date(rows$slice_start))
      finite_ends <- as.Date(rows$slice_end[nzchar(rows$slice_end)])
      end <- if (length(finite_ends)) max(finite_ends) else as.Date("2026-07-01")
      body <- list(
        cid = catalog_uuid,
        indicatorIds = as.list(rows$indicator_uuid),
        daCatalogId = "",
        das = list(list(text = "China", value = "000000000000")),
        showType = "1",
        dts = list(.nbs_encode_range(start, end, "M")),
        rootId = root_uuid
      )
      response <- .nbs_observation_request(client, body, "en")
      for (block in response$payload$data) {
        date <- date_from_code(.nbs_as_character_scalar(block$code))
        for (value in block$values) {
          indicator <- .nbs_as_character_scalar(value[["_id"]])
          member <- match(indicator, rows$indicator_uuid)
          if (is.na(member)) next
          parsed <- .nbs_parse_value(value$value)
          if (!is.na(parsed$value)) {
            key <- rows$series_id[[member]]
            present[[key]] <- c(present[[key]], date)
          }
        }
      }
      Sys.sleep(0.15)
    }
  }
  starts <- ends <- as.Date(rep(NA_character_, nrow(manifest)))
  keep <- logical(nrow(manifest))
  for (index in seq_len(nrow(manifest))) {
    dates <- as.Date(
      sort(unique(as.numeric(present[[manifest$series_id[[index]]]]))),
      origin = "1970-01-01"
    )
    # NBS monthly industrial financial tables omit January.  Do not turn that
    # published reporting rule into a false discontinuity.
    dates <- dates[format(dates, "%m") != "01"]
    if (!length(dates)) next
    expected <- .nbs_expected_period_sequence(
      min(dates), max(dates), "M", manifest$period_policy[[index]]
    )
    if (!identical(as.numeric(dates), as.numeric(expected))) next
    starts[[index]] <- min(dates)
    ends[[index]] <- max(dates)
    keep[[index]] <- TRUE
  }
  if (!any(keep)) stop("No industrial UUID has a continuous observed span.")
  manifest <- manifest[keep, , drop = FALSE]
  manifest$slice_start <- as.character(starts[keep])
  manifest$slice_end <- as.character(ends[keep])
  manifest$default_start <- manifest$slice_start
  rownames(manifest) <- NULL
  manifest
}

write_verified_existing <- function() {
  manifest_path <- file.path(output_dir, "nbs-industrial-series-manifest.csv.gz")
  dimensions_path <- file.path(output_dir, "nbs-industrial-dataset-dimensions.csv.gz")
  bindings_path <- file.path(output_dir, "nbs-industrial-bindings.csv.gz")
  datasets_path <- file.path(output_dir, "nbs-industrial-dataset-manifest.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  bindings <- utils::read.csv(bindings_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  dimensions <- utils::read.csv(dimensions_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  datasets <- utils::read.csv(datasets_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  selected <- if (length(verify_catalog)) {
    manifest_catalogue <- stats::setNames(manifest$catalog_uuid, manifest$series_id)
    bindings$series_id %in% names(manifest_catalogue)[
      manifest_catalogue %in% verify_catalog
    ]
  } else if (length(verify_dataset)) {
    if (!verify_dataset %in% datasets$dataset_id) stop("Unknown industrial dataset: ", verify_dataset)
    bindings$dataset_id == verify_dataset
  } else {
    rep(TRUE, nrow(bindings))
  }
  selected_series <- bindings$series_id[selected]
  verified <- if (refresh_datasets) {
    manifest[manifest$series_id %in% selected_series, , drop = FALSE]
  } else {
    continuous_available_rows(manifest[
      manifest$series_id %in% selected_series, , drop = FALSE
    ])
  }
  manifest <- rbind(
    manifest[!manifest$series_id %in% selected_series, , drop = FALSE],
    verified
  )
  manifest <- manifest[order(manifest$series_id), , drop = FALSE]
  bindings <- bindings[
    !bindings$series_id %in% selected_series |
      bindings$series_id %in% verified$series_id,
    , drop = FALSE
  ]
  dataset_by_series <- stats::setNames(bindings$dataset_id, bindings$series_id)
  dimensions <- dimensions[
    !dimensions$value %in% selected_series |
      dimensions$value %in% verified$series_id,
    , drop = FALSE
  ]
  index <- match(dimensions$value, manifest$series_id)
  dimensions$valid_from <- manifest$slice_start[index]
  dimensions$valid_to <- manifest$slice_end[index]
  dimensions$sort_order <- ave(seq_len(nrow(dimensions)), dimensions$dataset_id,
    FUN = seq_along)
  for (index in seq_len(nrow(datasets))) {
    if (!refresh_datasets && length(verify_catalog)) next
    if (length(verify_dataset) && datasets$dataset_id[[index]] != verify_dataset) next
    members <- dataset_by_series[manifest$series_id]
    members <- members == datasets$dataset_id[[index]]
    if (!any(members)) stop("Verification removed every tuple for ", datasets$dataset_id[[index]])
    datasets$expected_rows[[index]] <- sum(members)
    datasets$expected_cells[[index]] <- sum(members)
    datasets$coverage_en[[index]] <- paste(sum(members), "exact official portal leaves with continuous observed coverage")
    datasets$coverage_zh[[index]] <- paste(sum(members), "个具有连续观测覆盖的国家统计局门户精确叶节点")
    datasets$default_start[[index]] <- min(manifest$slice_start[members])
    datasets$available_to[[index]] <- max(manifest$slice_end[members])
  }
  csv(manifest, "nbs-industrial-series-manifest.csv.gz")
  csv(dimensions, "nbs-industrial-dataset-dimensions.csv.gz")
  csv(bindings, "nbs-industrial-bindings.csv.gz")
  csv(datasets, "nbs-industrial-dataset-manifest.csv")
  message("Verified ", nrow(verified), " industrial UUID leaves with continuous observed coverage.")
}

drop_unverified_datasets <- function() {
  manifest_path <- file.path(output_dir, "nbs-industrial-series-manifest.csv.gz")
  dimensions_path <- file.path(output_dir, "nbs-industrial-dataset-dimensions.csv.gz")
  bindings_path <- file.path(output_dir, "nbs-industrial-bindings.csv.gz")
  datasets_path <- file.path(output_dir, "nbs-industrial-dataset-manifest.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  bindings <- utils::read.csv(bindings_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  dimensions <- utils::read.csv(dimensions_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  datasets <- utils::read.csv(datasets_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  blocked <- c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2011",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY/GB4754_2017"
  )
  bindings <- bindings[!bindings$dataset_id %in% blocked, , drop = FALSE]
  retained <- bindings$series_id
  manifest <- manifest[manifest$series_id %in% retained, , drop = FALSE]
  dimensions <- dimensions[dimensions$value %in% retained, , drop = FALSE]
  datasets <- datasets[!datasets$dataset_id %in% blocked, , drop = FALSE]
  for (index in seq_len(nrow(datasets))) {
    members <- bindings$dataset_id == datasets$dataset_id[[index]]
    datasets$expected_rows[[index]] <- sum(members)
    datasets$expected_cells[[index]] <- sum(members)
    datasets$coverage_en[[index]] <- paste(sum(members), "exact official portal leaves with continuous observed coverage")
    datasets$coverage_zh[[index]] <- paste(sum(members), "个具有连续观测覆盖的国家统计局门户精确叶节点")
  }
  csv(manifest, "nbs-industrial-series-manifest.csv.gz")
  csv(dimensions, "nbs-industrial-dataset-dimensions.csv.gz")
  csv(bindings, "nbs-industrial-bindings.csv.gz")
  csv(datasets, "nbs-industrial-dataset-manifest.csv")
  message("Omitted challenge-blocked industrial eras: ", paste(blocked, collapse = ", "))
}

if (verify_existing) {
  write_verified_existing()
  quit(status = 0L)
}
if (drop_unverified) {
  drop_unverified_datasets()
  quit(status = 0L)
}

refresh_existing_units <- function() {
  manifest_path <- file.path(output_dir, "nbs-industrial-series-manifest.csv.gz")
  dimensions_path <- file.path(output_dir, "nbs-industrial-dataset-dimensions.csv.gz")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  dimensions <- utils::read.csv(dimensions_path, stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character", na.strings = character(),
    fileEncoding = "UTF-8")
  generic_units <- c("Official published unit", "Unit")
  affected <- manifest$unit_en %in% generic_units
  if (!any(affected)) {
    if (any(dimensions$unit_en %in% generic_units)) {
      stop("Industrial dimensions contain a generic unit absent from the manifest.")
    }
    message("All retained industrial UUID units are already explicit.")
    return(invisible(NULL))
  }
  units <- derive_official_units(manifest$title_en[affected])
  if (anyNA(units$unit_en) || anyNA(units$unit_zh)) {
    stop("A retained generic industrial UUID has an unrecognised official unit.")
  }
  manifest$unit_en[affected] <- units$unit_en
  manifest$unit_zh[affected] <- units$unit_zh
  matches <- match(dimensions$value, manifest$series_id)
  if (anyNA(matches)) stop("Industrial dimensions contain an unknown series ID.")
  dimensions$unit_en <- manifest$unit_en[matches]
  dimensions$unit_zh <- manifest$unit_zh[matches]
  if (any(manifest$unit_en %in% generic_units) ||
      any(dimensions$unit_en %in% generic_units)) {
    stop("Generic industrial UUID units remain after metadata refresh.")
  }
  csv(manifest, "nbs-industrial-series-manifest.csv.gz")
  csv(dimensions, "nbs-industrial-dataset-dimensions.csv.gz")
  message("Replaced ", sum(affected), " generic industrial UUID units from retained official title metadata.")
}

if (refresh_units) {
  refresh_existing_units()
  quit(status = 0L)
}

official_retry <- function(operation, identity) {
  for (attempt in seq_len(5L)) {
    value <- tryCatch(operation(), error = function(error) NULL)
    if (!is.null(value)) return(value)
    Sys.sleep(attempt)
  }
  stop("Could not retrieve official NBS metadata for ", identity)
}

rows <- lapply(configs, function(config) {
  leaves <- official_retry(
    function() nbs_tree("1", config$parent, "en"), config$parent
  )
  leaves <- leaves[leaves$is_leaf, c("catalog_uuid", "name"), drop = FALSE]
  if (!nrow(leaves)) stop("No official leaves for ", config$dataset_id)
  indicators <- do.call(rbind, lapply(seq_len(nrow(leaves)), function(index) {
    # The public portal intermittently serves an HTML rate-limit page.  A
    # generator must fail closed, but short polite retries prevent one such
    # response from producing a partial catalogue.
    found <- official_retry(
      function() nbs_indicators(leaves$catalog_uuid[[index]], "en"),
      leaves$catalog_uuid[[index]]
    )
    if (!nrow(found)) stop("No indicators for official leaf ", leaves$catalog_uuid[[index]])
    found$leaf_name <- leaves$name[[index]]
    Sys.sleep(0.2)
    found
  }))
  indicators <- indicators[nzchar(indicators$indicator_uuid) &
    nzchar(indicators$display_name), , drop = FALSE]
  if (!nrow(indicators) || anyDuplicated(indicators$indicator_uuid)) {
    stop("The official indicator metadata is empty or duplicated for ", config$dataset_id)
  }
  indicators$config_dataset_id <- config$dataset_id
  indicators$config_kind <- config$kind
  indicators$config_classification <- config$classification
  indicators$config_start <- config$start
  indicators$config_end <- config$end
  indicators$config_title_en <- config$title_en
  indicators$config_title_zh <- config$title_zh
  indicators
})
indicators <- do.call(rbind, rows)
if (anyDuplicated(paste(indicators$config_dataset_id, indicators$indicator_uuid))) {
  stop("An industrial panel contains duplicate indicator UUIDs.")
}
indicators <- correct_legacy_display_names(indicators)

series_id <- paste(
  "NBS/CN/M/INDUSTRIAL_PORTAL",
  toupper(vapply(indicators$config_classification, slug, character(1))),
  toupper(vapply(indicators$leaf_name, slug, character(1))),
  toupper(indicators$indicator_uuid), sep = "/"
)
if (anyDuplicated(series_id)) stop("Generated industrial series IDs are not unique.")
is_end_period <- grepl("month-end|month end", indicators$display_name,
  ignore.case = TRUE)
policy <- ifelse(is_end_period,
  "january_not_reported_february_first_period",
  "ytd_january_not_reported_february_combined")
aggregation <- ifelse(is_end_period, "single_period", "year_to_date")
units <- derive_official_units(indicators$display_name)
if (anyNA(units$unit_en)) {
  stop("An industrial UUID has an unrecognised official unit in its title metadata.")
}
manifest <- data.frame(
  manifest_version = version,
  series_id = series_id,
  title_en = indicators$display_name,
  title_zh = indicators$name,
  description_en = paste("Official NBS portal leaf:", indicators$display_name),
  description_zh = indicators$name,
  frequency = "M", root_uuid = root_uuid,
  catalog_uuid = indicators$catalog_uuid,
  indicator_uuid = indicators$indicator_uuid,
  slice_start = indicators$config_start,
  slice_end = indicators$config_end,
  default_start = indicators$config_start,
  classification_id = indicators$config_classification,
  stitch_policy = "classification_era_only_no_cross_era_stitching",
  period_policy = policy, aggregation_basis = aggregation,
  seasonal_adjustment = "not_seasonally_adjusted",
  series_note_en = "Exact official NBS portal UUID; this series belongs only to its published registration or industry-classification era.",
  series_note_zh = "国家统计局门户精确UUID；该序列仅属于其发布的登记注册类型或行业分类时期。",
  unit_en = units$unit_en, unit_zh = units$unit_zh, scale = "1",
  geography_code = "000000000000", geography_en = "China", geography_zh = "中国",
  source_url = paste0("https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid?cid=", indicators$catalog_uuid),
  source_note = "Latest/revised NBS National Data portal; generated from official UUID metadata 2026-08-11",
  stringsAsFactors = FALSE
)

dataset_rows <- lapply(configs, function(config) {
  members <- manifest$series_id[indicators$config_dataset_id == config$dataset_id]
  data.frame(
    manifest_version = version, dataset_id = config$dataset_id,
    release_id = "cn_nbs_industrial_profits", family = "industrial_profits",
    title_en = config$title_en, title_zh = config$title_zh,
    description_en = "Exact official NBS UUID panel. It is intentionally isolated from other classification or registration eras.",
    description_zh = "国家统计局精确UUID面板；有意与其他行业分类或登记注册类型时期隔离。",
    frequency = "M", route = "portal_uuid_panel", default_start = config$start,
    available_to = config$end,
    coverage_en = paste(length(members), "exact official portal leaves"),
    coverage_zh = paste(length(members), "个国家统计局门户精确叶节点"),
    required_dimensions = "series_id", panel_allowed = "TRUE",
    expected_rows = as.character(length(members)), expected_cells = as.character(length(members)),
    aggregation_basis = "mixed_span", seasonal_adjustment = "not_seasonally_adjusted",
    derived_method = "none", source_vintage = "latest_revised_database",
    series_note_en = "Select an exact series_id from valid tuples. No series is stitched across an official classification or registration break.",
    series_note_zh = "从有效元组中选择精确series_id；序列不跨官方分类或登记注册类型断点拼接。",
    unit_en = "Official published unit", unit_zh = "官方发布单位",
    source_url = "https://data.stats.gov.cn/",
    source_note = "Exact UUID bindings generated from official NBS portal metadata.",
    period_policy = "january_not_reported_february_first_period",
    stringsAsFactors = FALSE
  )
})
dataset_manifest <- do.call(rbind, dataset_rows)
dimensions <- data.frame(
  manifest_version = version, dataset_id = indicators$config_dataset_id,
  dimension = "series_id", value = series_id,
  label_en = indicators$display_name, label_zh = indicators$name,
  route_value = series_id,
  sort_order = ave(seq_along(series_id), indicators$config_dataset_id, FUN = seq_along),
  valid_from = indicators$config_start, valid_to = indicators$config_end,
  unit_en = units$unit_en, unit_zh = units$unit_zh, semantics = ifelse(is_end_period, "end_period", "year_to_date"),
  stringsAsFactors = FALSE
)
bindings <- data.frame(
  manifest_version = version, route_type = "exact",
  dataset_id = indicators$config_dataset_id,
  dimension_key = paste0("series_id=", utils::URLencode(series_id, reserved = TRUE)),
  category_code = "", series_id = series_id, stringsAsFactors = FALSE
)

if (!all(vapply(configs, function(config) any(dimensions$dataset_id == config$dataset_id), logical(1)))) {
  stop("At least one industrial panel has no safe default tuple.")
}
csv(manifest, "nbs-industrial-series-manifest.csv.gz")
csv(dataset_manifest, "nbs-industrial-dataset-manifest.csv")
csv(dimensions, "nbs-industrial-dataset-dimensions.csv.gz")
csv(bindings, "nbs-industrial-bindings.csv.gz")
message("Generated ", nrow(manifest), " exact industrial portal leaves across ", nrow(dataset_manifest), " non-stitching panels.")
