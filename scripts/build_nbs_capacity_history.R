#!/usr/bin/env Rscript

# Build the retained NBS industrial capacity-utilisation history.
#
# This is intentionally a small, separate archive.  NBS only began regular
# quarterly publication in 2017 Q4, while the detailed 19-industry panel
# retained elsewhere begins in 2018 Q2.  The annual national history published
# with the first quarterly releases runs from 2006.  Do not backfill either
# industry cells or YTD measures into this headline archive.

suppressPackageStartupMessages(library(digest))

root <- normalizePath(getwd(), mustWork = TRUE)
out <- file.path(root, "inst", "extdata")
raw_dir <- file.path(out, "nbs-capacity-history")
version <- "2026-08-11.1"

raw_sources <- data.frame(
  source_key = c(
    "annual_explainer", "annual_chart", "2018_q1", "2018_q4", "2019_q2"
  ),
  source_url = c(
    "https://www.stats.gov.cn/zs/tjws/tjzb/202301/t20230101_1903791.html",
    "https://www.stats.gov.cn/zs/tjws/tjzb/202301/W020250320610651488106_ORIGIN.png",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899897.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900206.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900369.html"
  ),
  retained_file = c(
    "annual-history-explainer.html", "annual-history-chart.png", "2018-q1.html",
    "2018-q4.html", "2019-q2.html"
  ),
  source_role = c(
    "NBS methodology/explanatory page establishing the 2006--22 annual chart and regular quarterly release boundary",
    "Official Figure 1, annual national industrial capacity-utilisation values for 2006--22",
    "Official 2018 Q1 national-economy release containing the national capacity-utilisation headline",
    "Official 2018 Q4 release; its year-on-year comparison recovers the published 2017 Q4 national headline",
    "Official 2019 Q2 release; its quarter-on-quarter comparison recovers the published 2019 Q1 national headline"
  ),
  stringsAsFactors = FALSE
)
raw_sources$path <- file.path(raw_dir, raw_sources$retained_file)
if (any(!file.exists(raw_sources$path))) {
  stop("Missing retained official NBS capacity-history capture(s).", call. = FALSE)
}
raw_sources$source_capture_sha256 <- vapply(raw_sources$path, function(path) {
  digest(path, algo = "sha256", serialize = FALSE, file = TRUE)
}, character(1L))

source_sha <- setNames(raw_sources$source_capture_sha256, raw_sources$source_key)
source_url <- setNames(raw_sources$source_url, raw_sources$source_key)

empty_record <- function() {
  as.list(setNames(rep("", 48L), c(
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
  )))
}

record <- function(dataset_id, release_period, observation_date, value, raw_value,
                   source_key, schema_version, published_value_type, row_label) {
  x <- empty_record()
  annual <- grepl("/A/", dataset_id, fixed = TRUE)
  x$dataset_id <- dataset_id
  x$category <- "national_industrial_total"
  x$measure <- "capacity_utilisation_percent"
  x$period_basis <- if (annual) "calendar_year" else "quarter"
  x$unit <- "percent"
  x$section <- "national_headline"
  x$row_label <- row_label
  x$table_index <- if (annual) 1L else 0L
  x$row_index <- if (annual) as.integer(format(as.Date(observation_date), "%Y")) - 2005L else 1L
  x$column_index <- 1L
  x$raw_value <- raw_value
  x$value <- value
  x$observation_date <- observation_date
  x$series_id <- paste(dataset_id, "NATIONAL_TOTAL", "CAPACITY_UTILISATION_PERCENT", sep = "/")
  x$reported_observation_date <- observation_date
  x$observation_period_start <- observation_date
  x$observation_period_end <- if (annual) {
    paste0(substr(observation_date, 1L, 4L), "-12-31")
  } else {
    as.character(seq(as.Date(observation_date), by = "3 months", length.out = 2L)[[2L]] - 1L)
  }
  x$published_value_type <- published_value_type
  x$schema_version <- schema_version
  x$classification_id <- schema_version
  x$coverage_version <- "national_headline_only"
  x$seasonal_adjustment <- "not_seasonally_adjusted"
  x$manifest_version <- version
  x$release_period <- release_period
  x$release_source_url <- source_url[[source_key]]
  x$dimension_label_en <- "National industrial capacity utilisation"
  x$dimension_label_zh <- "全国工业产能利用率"
  x$unit_en <- "Percent"
  x$unit_zh <- "百分比"
  as.data.frame(x, stringsAsFactors = FALSE)
}

annual_years <- 2006:2022
# Transcribed exactly from the retained official NBS Figure 1.  The image is
# deliberately retained because NBS publishes these observations as a chart,
# not as a machine-readable table on the explanatory page.
annual_values <- c(
  79.1, 81.3, 77.2, 73.1, 79.2, 80.4, 77.5, 75.8, 75.6,
  74.3, 73.3, 77.0, 76.5, 76.6, 74.5, 77.5, 75.6
)
annual <- do.call(rbind, lapply(seq_along(annual_years), function(i) record(
  "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY",
  "2006-2022", paste0(annual_years[[i]], "-01-01"), annual_values[[i]],
  format(annual_values[[i]], nsmall = 1L), "annual_chart",
  "industrial_capacity_annual_national_2006_2022",
  "official_chart_value", paste0(annual_years[[i]], "年度工业产能利用率")
)))

# 2017 Q4 and 2019 Q1 are retained only as explicitly published comparisons in
# the official subsequent quarterly releases.  The source page itself states
# the exact prior-period movement, so the recovered displayed value is not a
# modelled or interpolated observation.
quarterly <- rbind(
  record(
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019",
    "2017-Q4", "2017-10-01", 78.0, "78.0", "2018_q4",
    "industrial_capacity_quarterly_national_headline_pre_detail",
    "official_subsequent_release_comparison", "全国工业产能利用率（2017年四季度）"
  ),
  record(
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019",
    "2018-Q1", "2018-01-01", 76.5, "76.5", "2018_q1",
    "industrial_capacity_quarterly_national_headline_pre_detail",
    "official_headline_value", "全国工业产能利用率（2018年一季度）"
  ),
  record(
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019",
    "2019-Q1", "2019-01-01", 75.9, "75.9", "2019_q2",
    "industrial_capacity_quarterly_national_headline_pre_detail",
    "official_subsequent_release_comparison", "全国工业产能利用率（2019年一季度）"
  )
)

observations <- rbind(annual, quarterly)
archive_path <- file.path(out, "nbs-capacity-history-observations.csv.gz")
connection <- gzfile(archive_path, "wb")
utils::write.csv(observations, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8")
close(connection)
archive_sha <- digest(archive_path, algo = "sha256", serialize = FALSE, file = TRUE)

release_index <- data.frame(
  manifest_version = version,
  dataset_id = c(
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY",
    rep("NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019", 3L)
  ),
  period = c("2006-2022", "2017-Q4", "2018-Q1", "2019-Q1"),
  reference_period_start = c("2006-01-01", "2017-10-01", "2018-01-01", "2019-01-01"),
  reference_period_end = c("2022-12-31", "2017-12-31", "2018-03-31", "2019-03-31"),
  release_date = c("2023-01-01", "2018-01-19", "2018-04-17", "2019-04-17"),
  source_url = unname(c(source_url[["annual_explainer"]], source_url[c("2018_q4", "2018_q1", "2019_q2")])),
  basket_version = "",
  index_base = "",
  expected_press_cells = c(17L, 1L, 1L, 1L),
  schema_version = c(
    "industrial_capacity_annual_national_2006_2022",
    rep("industrial_capacity_quarterly_national_headline_pre_detail", 3L)
  ),
  storage = "snapshot",
  snapshot_sha256 = archive_sha,
  source_capture_sha256 = unname(c(source_sha[["annual_chart"]], source_sha[c("2018_q4", "2018_q1", "2019_q2")])),
  stringsAsFactors = FALSE
)
utils::write.csv(release_index,
  file.path(out, "nbs-capacity-history-release-index.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
source_index <- raw_sources[c(
  "source_key", "source_url", "retained_file", "source_role", "source_capture_sha256"
)]
utils::write.csv(source_index,
  file.path(out, "nbs-capacity-history-source-index.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

cat(sprintf("Wrote %d capacity-history observations; SHA-256 %s\n", nrow(observations), archive_sha))
