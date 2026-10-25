#!/usr/bin/env Rscript

# Build the frozen 2010--11 demographic-communique supplement from two
# retained official NBS captures.  This is deliberately a narrow, auditable
# maintenance path: it validates the exact publication wording/table before
# it writes the normalized archive; it never tries to fill unavailable 2010
# fields from another source.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: build_nbs_demography_history.R SOURCE_DIR OUTPUT_DIR", call. = FALSE)
}
source_dir <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

captures <- data.frame(
  period = c("2010", "2011"),
  filename = c("2010.html", "2011.html"),
  source_url = c(
    "https://www.stats.gov.cn/sj/tjgb/ndtjgb/qgndtjgb/202302/t20230206_1901954.html",
    "https://www.stats.gov.cn/english/PressRelease/201202/t20120222_72118.html"
  ),
  release_date = c("2011-02-28", "2012-02-22"),
  source_capture_sha256 = c(
    "4e6bc7cbd0d2df2308e5932c113c2d671055b2dacfa675a581c2426e74a21709",
    "46690b985daf1bd9abc528798c3d097aa00d5ab86ad47fb2d63fe64facdd7205"
  ),
  schema_version = c(
    "demography-total-population-preliminary-2010-v1",
    "demography-total-population-age-15-59-2011-v1"
  ),
  stringsAsFactors = FALSE
)

sha256 <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
paths <- file.path(source_dir, captures$filename)
if (any(!file.exists(paths)) || !identical(
  unname(vapply(paths, sha256, character(1L))), captures$source_capture_sha256
)) {
  stop("Official demographic captures are missing or do not match pinned SHA-256 values.", call. = FALSE)
}

raw_2010 <- paste(readLines(paths[[1L]], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
text_2010 <- rvest::html_text2(xml2::read_html(raw_2010))
if (!grepl("年末全国总人口(?:\\[[0-9]+\\])?\\s*134100\\s*万人", text_2010, perl = TRUE) ||
    !grepl("初步预计", text_2010, fixed = TRUE)) {
  stop("2010 capture did not contain the preliminary 134100 headline population.", call. = FALSE)
}
doc_2011 <- xml2::read_html(paths[[2L]])
tables <- rvest::html_table(doc_2011, fill = TRUE)
population <- Filter(function(table) {
  all(c("National Total", "Aged 15-59", "Aged 60 and over") %in% table[[1L]])
}, tables)
if (length(population) != 1L) {
  stop("2011 capture did not contain one identifiable population table.", call. = FALSE)
}
population <- as.data.frame(population[[1L]], stringsAsFactors = FALSE)
cell <- function(label, column) {
  labels <- trimws(gsub("[[:space:]\u00a0]+", " ", population[[1L]], perl = TRUE))
  matches <- which(labels == label)
  if (length(matches) != 1L) stop("2011 population row was not unique: ", label, call. = FALSE)
  value <- trimws(population[[column]][[matches]])
  if (!grepl("^[0-9.]+$", value)) stop("2011 population cell was not numeric: ", label, call. = FALSE)
  value
}
values <- c(
  total_population_year_end = cell("National Total", 2L),
  urbanisation_percent = cell("Of which: Urban", 3L),
  age_15_59_year_end = cell("Aged 15-59", 2L),
  age_60_plus_year_end = cell("Aged 60 and over", 2L)
)
prose <- rvest::html_text2(rvest::html_element(doc_2011, "body"))
births <- regmatches(prose, regexec("([0-9.]+) million births", prose, perl = TRUE))[[1L]][[2L]]
deaths <- regmatches(prose, regexec("([0-9.]+) million deaths", prose, perl = TRUE))[[1L]][[2L]]
if (!identical(values, c(total_population_year_end = "134735", urbanisation_percent = "51.3", age_15_59_year_end = "94072", age_60_plus_year_end = "18499")) ||
    !identical(births, "16.04") || !identical(deaths, "9.60")) {
  stop("2011 demographic values did not match the reviewed official release.", call. = FALSE)
}

columns <- c(
  "dataset_id", "category", "industry", "city_code", "city_label", "market",
  "size_band", "comparison", "index_base", "region", "measure", "period_basis",
  "residence", "nominal_real", "unit_type", "registration", "occupation",
  "product_code", "specification", "unit", "section", "row_label", "table_index",
  "row_index", "column_index", "raw_value", "value", "observation_date", "series_id",
  "reported_observation_date", "observation_period_start", "observation_period_end",
  "comparison_period_start", "comparison_period_end", "comparison_basis",
  "published_value_type", "schema_version", "basket_version", "classification_id",
  "coverage_version", "seasonal_adjustment", "manifest_version", "release_period",
  "release_source_url", "dimension_label_en", "dimension_label_zh", "unit_en", "unit_zh"
)
blank <- function(n) as.data.frame(setNames(replicate(length(columns), rep("", n), simplify = FALSE), columns), stringsAsFactors = FALSE)
make_rows <- function(period, measures, raw, values, schema, source_url) {
  rows <- blank(length(measures))
  rows$dataset_id <- "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE"
  rows$measure <- measures
  rows$period_basis <- "annual"
  rows$unit <- ifelse(measures == "urbanisation_percent", "Percent", "10,000 persons")
  rows$row_label <- measures
  rows$table_index <- ifelse(period == "2011", 15L, 0L)
  rows$row_index <- seq_along(measures)
  rows$column_index <- 1L
  rows$raw_value <- raw
  rows$value <- values
  rows$schema_version <- schema
  rows$basket_version <- paste0("population-communique-", period)
  rows$classification_id <- schema
  rows$seasonal_adjustment <- "not_applicable"
  rows$manifest_version <- "2026-08-11.1"
  rows$release_period <- period
  rows$release_source_url <- source_url
  rows$unit_en <- rows$unit
  rows$unit_zh <- ifelse(measures == "urbanisation_percent", "百分比", "万人")
  rows
}
rows <- rbind(
  make_rows("2010", "total_population_year_end", "134100", 134100,
    captures$schema_version[[1L]], captures$source_url[[1L]]),
  make_rows("2011", c("total_population_year_end", "births_level", "deaths_level",
    "urbanisation_percent", "age_15_59_year_end", "age_60_plus_year_end"),
    c(values[["total_population_year_end"]], births, deaths,
      values[["urbanisation_percent"]], values[["age_15_59_year_end"]],
      values[["age_60_plus_year_end"]]),
    c(as.numeric(values[["total_population_year_end"]]), as.numeric(births) * 100,
      as.numeric(deaths) * 100, as.numeric(values[["urbanisation_percent"]]),
      as.numeric(values[["age_15_59_year_end"]]), as.numeric(values[["age_60_plus_year_end"]])),
    captures$schema_version[[2L]], captures$source_url[[2L]])
)
archive_path <- file.path(output_dir, "nbs-demography-history-observations.csv.gz")
archive_connection <- gzfile(archive_path, "wt")
utils::write.csv(rows, archive_connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(archive_connection)
archive_sha <- sha256(archive_path)
index <- data.frame(
  manifest_version = "2026-08-11.1",
  dataset_id = "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
  period = captures$period,
  reference_period_start = paste0(captures$period, "-01-01"),
  reference_period_end = paste0(captures$period, "-12-31"),
  release_date = captures$release_date,
  source_url = captures$source_url,
  basket_version = paste0("population-communique-", captures$period),
  index_base = "",
  expected_press_cells = c(1L, 6L),
  schema_version = captures$schema_version,
  storage = "snapshot",
  snapshot_sha256 = archive_sha,
  source_capture_sha256 = captures$source_capture_sha256,
  stringsAsFactors = FALSE
)
utils::write.csv(index, file.path(output_dir, "nbs-demography-history-release-index.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("Wrote 7 exact cells; archive SHA-256 %s\n", archive_sha))
