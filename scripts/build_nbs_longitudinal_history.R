#!/usr/bin/env Rscript

# Build chart-ready longitudinal wage and migrant-worker series from the
# source-faithful retained NBS release archives. The raw table-cell datasets
# remain unchanged and available for audit. This supplement only joins exact
# like-for-like observations, preserves the selected source vintage for every
# point, and never interpolates or backcasts a value.

suppressPackageStartupMessages({
  library(digest)
  library(rvest)
})

root <- normalizePath(getwd(), mustWork = TRUE)
out <- file.path(root, "inst", "extdata")
version <- "2026-08-18.1"
wage_id <- "NBS/CN/A/WAGES/LONGITUDINAL"
migrant_id <- "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"

read_character_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
}

bind_rows <- function(frames) {
  frames <- Filter(function(x) !is.null(x) && nrow(x), frames)
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- ""
    x[columns]
  })
  do.call(rbind, frames)
}

clean_columns <- function(x) {
  x[, !grepl("^NA(?:[.]|$)", names(x)), drop = FALSE]
}

core <- clean_columns(read_character_csv(file.path(
  out, "nbs-press-observations.csv.gz"
)))
wage_history <- clean_columns(read_character_csv(file.path(
  out, "nbs-wages-history-observations.csv.gz"
)))
migrant_history <- clean_columns(read_character_csv(file.path(
  out, "nbs-migrant-history-observations.csv.gz"
)))
release_index <- read_character_csv(file.path(out, "nbs-release-index.csv"))
wage_index <- read_character_csv(file.path(
  out, "nbs-wages-history-release-index.csv"
))
migrant_index <- read_character_csv(file.path(
  out, "nbs-migrant-history-release-index.csv"
))

observation_columns <- unique(c(
  names(core), "breakdown", "series", "canonical_label"
))
to_observation_shape <- function(x) {
  for (column in setdiff(observation_columns, names(x))) x[[column]] <- ""
  x[observation_columns]
}

# ---- Wages ---------------------------------------------------------------

macro_regions <- c("eastern", "central", "western", "northeastern")

prepare_wage_rows <- function(rows, current = FALSE) {
  if (current) {
    rows <- rows[
      rows$dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES" &
        rows$unit_type %in% c("non_private", "private") &
        rows$measure %in% c(
          "current_year_average_wage", "prior_year_average_wage",
          "nominal_growth_rate"
        ),
      , drop = FALSE
    ]
    rows$measure[rows$measure %in% c(
      "current_year_average_wage", "prior_year_average_wage"
    )] <- "average_wage"
  } else {
    rows <- rows[rows$measure %in% c(
      "average_wage", "nominal_growth_rate"
    ), , drop = FALSE]
  }

  is_region <- rows$region %in% c("total", macro_regions)
  is_industry <- nzchar(rows$industry) &
    rows$industry != "not_applicable"
  rows <- rows[is_region | is_industry, , drop = FALSE]
  is_region <- rows$region %in% c("total", macro_regions)
  is_industry <- nzchar(rows$industry) &
    rows$industry != "not_applicable"

  rows$breakdown <- ifelse(
    (is_region & rows$region == "total") |
      (is_industry & rows$industry == "total"),
    "overall",
    ifelse(is_region, "region", "industry")
  )
  rows$category <- ifelse(
    rows$breakdown == "overall", "total",
    ifelse(rows$breakdown == "region", rows$region, rows$industry)
  )
  rows$dataset_id <- wage_id
  rows$release_period <- if (current) {
    rows$release_period
  } else {
    paste(rows$release_period, rows$unit_type, sep = "-")
  }
  rows$period_basis <- "annual"
  rows$seasonal_adjustment <- "not_applicable"
  rows$series_id <- ""
  rows$series <- ""
  rows$manifest_version <- version
  rows
}

wages <- bind_rows(list(
  prepare_wage_rows(wage_history, current = FALSE),
  prepare_wage_rows(core, current = TRUE)
))

# The 2010 releases state the national totals in their prose but do not repeat
# them in the regional/industry table body. Add those exact published values
# so the ordinary national level and growth series begin in 2010.
headline_2010 <- data.frame(
  unit_type = c("non_private", "non_private", "private", "private"),
  measure = c(
    "average_wage", "nominal_growth_rate",
    "average_wage", "nominal_growth_rate"
  ),
  value = c("37147", "13.5", "20759", "14.1"),
  unit = c("Yuan", "Percent", "Yuan", "Percent"),
  unit_en = c("Yuan", "Percent", "Yuan", "Percent"),
  unit_zh = c("元", "%", "元", "%"),
  stringsAsFactors = FALSE
)
for (index in seq_len(nrow(headline_2010))) {
  unit_type <- headline_2010$unit_type[[index]]
  template <- wages[
    wages$unit_type == unit_type & wages$release_period ==
      paste("2010", unit_type, sep = "-"),
    , drop = FALSE
  ][1L, , drop = FALSE]
  if (!nrow(template)) stop("Missing 2010 wage source template.", call. = FALSE)
  template$breakdown <- "overall"
  template$category <- "total"
  template$region <- "total"
  template$industry <- "not_applicable"
  template$registration <- "not_applicable"
  template$measure <- headline_2010$measure[[index]]
  template$unit <- headline_2010$unit[[index]]
  template$unit_en <- headline_2010$unit_en[[index]]
  template$unit_zh <- headline_2010$unit_zh[[index]]
  template$raw_value <- headline_2010$value[[index]]
  template$value <- headline_2010$value[[index]]
  template$observation_date <- "2010-01-01"
  template$reported_observation_date <- "2010-01-01"
  template$observation_period_start <- "2010-01-01"
  template$observation_period_end <- "2010-12-31"
  template$row_label <- "Official national total stated in release text"
  template$section <- "release_text_headline"
  template$table_index <- "0"
  template$row_index <- as.character(index)
  template$column_index <- "0"
  wages <- bind_rows(list(wages, template))
}

wages <- to_observation_shape(wages)
wages$value <- suppressWarnings(as.numeric(wages$value))
if (any(!is.finite(wages$value))) {
  stop("Longitudinal wage values are invalid.", call. = FALSE)
}

# Prefer the latest retained official publication when a later release repeats
# the same observation (for example, a prior-year comparison column).
wage_key <- paste(
  wages$unit_type, wages$breakdown, wages$category, wages$measure,
  wages$observation_date, sep = "\r"
)
wage_release_order <- suppressWarnings(as.integer(substr(
  wages$release_period, 1L, 4L
)))
order_index <- order(wage_key, wage_release_order, wages$release_period)
wages <- wages[order_index, , drop = FALSE]
wage_key <- wage_key[order_index]
wages <- wages[!duplicated(wage_key, fromLast = TRUE), , drop = FALSE]

if (anyDuplicated(paste(
  wages$unit_type, wages$breakdown, wages$category, wages$measure,
  wages$observation_date, sep = "\r"
))) {
  stop("Longitudinal wage series contain duplicate dates.", call. = FALSE)
}

# ---- Migrant workers -----------------------------------------------------

canonical_migrant_label <- function(label) {
  label <- sub("^20[0-9]{2} — ", "", label)
  label <- sub("^Table [0-9]+: ", "", label)
  label <- sub(" — cell r.*$", "", label)
  parts <- strsplit(label, " — ", fixed = TRUE)
  label <- vapply(parts, function(values) {
    values <- values[!grepl("^20[0-9]{2}$", values)]
    paste(values, collapse = " — ")
  }, character(1L))
  replacements <- c(
    "Migrant-worker population" = "Migrant-worker numbers",
    "Migrant-worker numbers and composition" = "Migrant-worker numbers",
    "Employment industry distribution of migrant workers" =
      "Industry distribution of migrant workers",
    "Main industry distribution of migrant workers" =
      "Industry distribution of migrant workers",
    "Monthly income per migrant worker and growth, by industry" =
      "Average monthly migrant-worker income and growth, by industry",
    "Of which: migrant workers working away from home" =
      "Migrant workers working away from their home township",
    "Of which: manufacturing" = "Manufacturing",
    "Of which: wholesale and retail trade" = "Wholesale and retail trade"
  )
  for (from in names(replacements)) {
    label <- gsub(from, replacements[[from]], label, fixed = TRUE)
  }
  gsub("[[:space:]]+", " ", trimws(label), perl = TRUE)
}

migrant <- migrant_history
migrant$canonical_label <- canonical_migrant_label(migrant$dimension_label_en)
migrant$release_rank <- suppressWarnings(as.integer(migrant$release_period))
migrant <- to_observation_shape(migrant)
migrant$canonical_label <- canonical_migrant_label(
  migrant_history$dimension_label_en
)
migrant$release_rank <- suppressWarnings(as.integer(
  migrant_history$release_period
))

# Map the current report's repeated age, industry-share and income tables onto
# the same semantic labels as the historical reports. Single-year cells that
# cannot be matched remain available in the source-faithful 2025 table dataset.
current_migrant <- core[
  core$dataset_id == "NBS/CN/A/MIGRANT_WORKERS", , drop = FALSE
]

add_current_row <- function(row, label, year, unit = row$unit[[1L]]) {
  row$dataset_id <- migrant_id
  row$release_period <- "2025"
  row$observation_date <- sprintf("%d-01-01", year)
  row$reported_observation_date <- row$observation_date
  row$observation_period_start <- row$observation_date
  row$observation_period_end <- sprintf("%d-12-31", year)
  row$period_basis <- "annual"
  row$unit <- unit
  row$manifest_version <- version
  row$canonical_label <- label
  row$release_rank <- 2025L
  row
}

age_labels <- c(
  "16-20岁" = "16-20 years", "21-30岁" = "21-30 years",
  "31-40岁" = "31-40 years", "41-50岁" = "41-50 years",
  "50岁以上" = "50 years and over"
)
age_rows <- current_migrant[current_migrant$table_index == "3", , drop = FALSE]

industry_rows <- current_migrant[
  current_migrant$table_index == "4" &
    current_migrant$column_index %in% c("2", "3"),
  , drop = FALSE
]
industry_labels <- c(
  "第一产业" = "Primary industry", "第二产业" = "Secondary industry",
  "其中：制造业" = "Manufacturing", "建筑业" = "Construction",
  "第三产业" = "Tertiary industry",
  "其中：批发和零售业" = "Wholesale and retail trade",
  "交通运输仓储和邮政业" = "Transport, storage and postal services",
  "住宿餐饮业" = "Accommodation and catering",
  "居民服务修理和其他服务业" =
    "Resident services, repairs and other services"
)
income_rows <- current_migrant[
  current_migrant$table_index == "5", , drop = FALSE
]
income_labels <- c(
  "全部" = "Total", "制造业" = "Manufacturing",
  "建筑业" = "Construction", "批发和零售业" = "Wholesale and retail trade",
  "交通运输仓储和邮政业" = "Transport, storage and postal services",
  "住宿餐饮业" = "Accommodation and catering",
  "居民服务修理和其他服务业" =
    "Resident services, repairs and other services"
)
current_mapped <- list()
append_mapped <- function(row, label, year) {
  row <- add_current_row(row, label, year)
  row <- to_observation_shape(row)
  row$canonical_label <- label
  row$release_rank <- 2025L
  current_mapped[[length(current_mapped) + 1L]] <<- row
}
for (index in seq_len(nrow(age_rows))) {
  year <- suppressWarnings(as.integer(sub(".* - (20[0-9]{2})$", "\\1",
    age_rows$dimension_label_en[[index]])))
  label <- age_labels[[age_rows$row_label[[index]]]]
  if (!is.null(label) && !is.na(year)) append_mapped(
    age_rows[index, , drop = FALSE],
    paste("Age composition of migrant workers", label, sep = " — "), year
  )
}
for (index in seq_len(nrow(industry_rows))) {
  label <- industry_labels[[industry_rows$row_label[[index]]]]
  if (!is.null(label)) append_mapped(
    industry_rows[index, , drop = FALSE],
    paste("Industry distribution of migrant workers", label, sep = " — "),
    if (industry_rows$column_index[[index]] == "2") 2024L else 2025L
  )
}
for (index in seq_len(nrow(income_rows))) {
  category <- income_labels[[income_rows$row_label[[index]]]]
  if (is.null(category)) next
  label <- paste(
    "Average monthly migrant-worker income and growth, by industry",
    category, sep = " — "
  )
  if (income_rows$column_index[[index]] == "4") {
    label <- paste(label, "Growth rate", sep = " — ")
  }
  row <- income_rows[index, , drop = FALSE]
  if (row$column_index[[1L]] != "4") {
    row$unit <- "Yuan per worker per month"
    row$unit_en <- "Yuan per worker per month"
    row$unit_zh <- "元/人/月"
  }
  append_mapped(
    row, label,
    if (income_rows$column_index[[index]] == "2") 2024L else 2025L
  )
}

migrant <- bind_rows(c(list(migrant), current_mapped))

# From 2016 onward, the three population totals are published in report prose
# rather than in the retained numeric tables. Extract the exact values from
# the frozen official HTML captures; the 2025 values use the current retained
# release capture represented by the core archive.
extract_first_integer <- function(text, pattern, source) {
  match <- regexec(pattern, text, perl = TRUE)
  captures <- regmatches(text, match)[[1L]]
  if (length(captures) != 2L) {
    stop("Could not extract migrant-worker headline from ", source,
      call. = FALSE)
  }
  as.numeric(captures[[2L]])
}

headline_rows <- list()
for (year in 2016:2024) {
  source <- file.path(
    out, "nbs-migrant-history", paste0(year, ".html")
  )
  text <- rvest::html_text2(rvest::read_html(source))
  values <- c(
    "Migrant-worker numbers — Migrant workers, total" =
      extract_first_integer(
        text,
        paste0(year, "年(?:全国)?农民工总量(?:达到|为)?([0-9]+)万人"),
        source
      ),
    "Migrant-worker numbers — Migrant workers working away from their home township" =
      extract_first_integer(text, "外出农民工([0-9]+)万人", source),
    "Migrant-worker numbers — Local migrant workers" =
      extract_first_integer(text, "本地农民工([0-9]+)万人", source)
  )
  headline_rows[[length(headline_rows) + 1L]] <- data.frame(
    year = year, label = names(values), value = unname(values),
    stringsAsFactors = FALSE
  )
}
headline_rows[[length(headline_rows) + 1L]] <- data.frame(
  year = 2025L,
  label = c(
    "Migrant-worker numbers — Migrant workers, total",
    "Migrant-worker numbers — Migrant workers working away from their home township",
    "Migrant-worker numbers — Local migrant workers"
  ),
  value = c(30115, 18006, 12109),
  stringsAsFactors = FALSE
)
headline_migrant <- do.call(rbind, headline_rows)

for (index in seq_len(nrow(headline_migrant))) {
  label <- headline_migrant$label[[index]]
  year <- headline_migrant$year[[index]]
  template <- migrant_history[1L, , drop = FALSE]
  template <- to_observation_shape(template)
  template$dataset_id <- migrant_id
  template$release_period <- as.character(year)
  template$observation_date <- sprintf("%d-01-01", year)
  template$reported_observation_date <- template$observation_date
  template$observation_period_start <- template$observation_date
  template$observation_period_end <- sprintf("%d-12-31", year)
  template$raw_value <- as.character(headline_migrant$value[[index]])
  template$value <- as.character(headline_migrant$value[[index]])
  template$unit <- "10,000 persons"
  template$unit_en <- "10,000 persons"
  template$unit_zh <- "万人"
  template$period_basis <- "annual"
  template$section <- "release_text_headline"
  template$row_label <- "Official national total stated in release text"
  template$table_index <- "0"
  template$row_index <- as.character(index)
  template$column_index <- "0"
  source_record <- if (year == 2025L) {
    release_index[
      release_index$dataset_id == "NBS/CN/A/MIGRANT_WORKERS" &
        release_index$period == "2025", , drop = FALSE
    ]
  } else {
    migrant_index[
      migrant_index$period == as.character(year), , drop = FALSE
    ]
  }
  if (nrow(source_record) != 1L) {
    stop("Missing exact migrant-worker release provenance for ", year,
      call. = FALSE)
  }
  template$release_source_url <- source_record$source_url[[1L]]
  template$schema_version <- source_record$schema_version[[1L]]
  template$classification_id <- source_record$schema_version[[1L]]
  template$basket_version <- source_record$basket_version[[1L]]
  template$index_base <- source_record$index_base[[1L]]
  template$source_capture_sha256 <-
    source_record$source_capture_sha256[[1L]]
  template$manifest_version <- version
  template$canonical_label <- label
  template$release_rank <- 2025L
  migrant <- bind_rows(list(migrant, template))
}

migrant$dataset_id <- migrant_id
migrant$manifest_version <- version
migrant$value <- suppressWarnings(as.numeric(migrant$value))
migrant$release_rank <- suppressWarnings(as.integer(migrant$release_period))
if (any(!is.finite(migrant$value)) || any(!nzchar(migrant$canonical_label))) {
  stop("Longitudinal migrant-worker values or labels are invalid.", call. = FALSE)
}

# A label must have one semantic unit. Fail closed if title normalisation would
# otherwise merge a level and a rate.
label_units <- split(migrant$unit, migrant$canonical_label)
bad_units <- names(Filter(function(x) length(unique(x)) != 1L, label_units))
if (length(bad_units)) {
  unit_suffix <- c(
    "10,000 persons" = "Number (10,000 persons)",
    "Percent" = "Share or rate (percent)",
    "Percentage points" = "Change (percentage points)",
    "Yuan" = "Amount (yuan)",
    "Yuan per worker per month" = "Amount (yuan per worker per month)",
    "Yuan per person per month" = "Amount (yuan per person per month)",
    "Months per year" = "Duration (months per year)",
    "Days per month" = "Duration (days per month)",
    "Hours per day" = "Duration (hours per day)"
  )
  split_rows <- migrant$canonical_label %in% bad_units
  suffix <- unname(unit_suffix[migrant$unit[split_rows]])
  if (anyNA(suffix)) {
    stop("An unreviewed migrant-worker unit needs a label suffix.", call. = FALSE)
  }
  migrant$canonical_label[split_rows] <- paste(
    migrant$canonical_label[split_rows], suffix, sep = " — "
  )
  label_units <- split(migrant$unit, migrant$canonical_label)
  bad_units <- names(Filter(function(x) length(unique(x)) != 1L, label_units))
  if (length(bad_units)) {
    stop(
      "Migrant labels still resolve to conflicting units: ",
      paste(utils::head(bad_units, 10L), collapse = "; "), call. = FALSE
    )
  }
}

migrant_key <- paste(
  migrant$canonical_label, migrant$observation_date, sep = "\r"
)
order_index <- order(migrant_key, migrant$release_rank)
migrant <- migrant[order_index, , drop = FALSE]
migrant_key <- migrant_key[order_index]
migrant <- migrant[!duplicated(migrant_key, fromLast = TRUE), , drop = FALSE]

date_counts <- tapply(
  migrant$observation_date, migrant$canonical_label,
  function(x) length(unique(x))
)
migrant <- migrant[
  migrant$canonical_label %in% names(date_counts)[date_counts >= 8L],
  , drop = FALSE
]
migrant$series <- paste0(
  "mw_",
  vapply(migrant$canonical_label, digest, character(1L),
    algo = "sha256", serialize = FALSE),
  USE.NAMES = FALSE
)
migrant$series <- substr(migrant$series, 1L, 19L)
migrant$measure <- "longitudinal_indicator"
migrant$period_basis <- "annual"
migrant$seasonal_adjustment <- "not_applicable"
migrant <- to_observation_shape(migrant)

if (anyDuplicated(paste(migrant$series, migrant$observation_date, sep = "\r"))) {
  stop("Longitudinal migrant-worker series contain duplicate dates.", call. = FALSE)
}

# ---- Shared archive and release index -----------------------------------

wages <- wages[order(
  wages$unit_type, wages$breakdown, wages$category, wages$measure,
  wages$observation_date
), , drop = FALSE]
migrant <- migrant[order(migrant$series, migrant$observation_date), , drop = FALSE]
observations <- bind_rows(list(wages, migrant))
observations <- observations[, unique(c(observation_columns, setdiff(
  names(observations), observation_columns
))), drop = FALSE]

archive_path <- file.path(out, "nbs-longitudinal-observations.csv.gz")
connection <- gzfile(archive_path, "wb")
utils::write.csv(
  observations, connection, row.names = FALSE, na = "",
  fileEncoding = "UTF-8"
)
close(connection)
archive_hash <- digest(archive_path, algo = "sha256", file = TRUE,
  serialize = FALSE)

wage_history_sources <- transform(
  wage_index,
  source_dataset_id = dataset_id,
  source_period = period
)
wage_history_sources$period <- paste(
  wage_history_sources$period,
  ifelse(
    grepl("NON_PRIVATE", wage_history_sources$source_dataset_id, fixed = TRUE),
    "non_private", "private"
  ),
  sep = "-"
)
wage_sources <- bind_rows(list(
  wage_history_sources,
  transform(
    release_index[
      release_index$dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES", , drop = FALSE
    ],
    source_dataset_id = dataset_id,
    source_period = period
  )
))
wage_sources$period <- tolower(wage_sources$period)
wage_sources$dataset_id <- wage_id

migrant_sources <- bind_rows(list(
  transform(
    migrant_index,
    source_dataset_id = dataset_id,
    source_period = period
  ),
  transform(
    release_index[
      release_index$dataset_id == "NBS/CN/A/MIGRANT_WORKERS" &
        release_index$period == "2025", , drop = FALSE
    ],
    source_dataset_id = dataset_id,
    source_period = period
  )
))
migrant_sources$dataset_id <- migrant_id

sources <- bind_rows(list(wage_sources, migrant_sources))
sources <- sources[sources$period %in% unique(observations$release_period), , drop = FALSE]
sources$manifest_version <- version
sources$expected_press_cells <- vapply(seq_len(nrow(sources)), function(index) {
  sum(
    observations$dataset_id == sources$dataset_id[[index]] &
      observations$release_period == sources$period[[index]]
  )
}, integer(1L))
sources$reference_period_start <- vapply(seq_len(nrow(sources)), function(index) {
  rows <- observations[
    observations$dataset_id == sources$dataset_id[[index]] &
      observations$release_period == sources$period[[index]],
    , drop = FALSE
  ]
  min(rows$observation_date)
}, character(1L))
sources$reference_period_end <- vapply(seq_len(nrow(sources)), function(index) {
  rows <- observations[
    observations$dataset_id == sources$dataset_id[[index]] &
      observations$release_period == sources$period[[index]],
    , drop = FALSE
  ]
  paste0(substr(max(rows$observation_date), 1L, 4L), "-12-31")
}, character(1L))
sources$storage <- "snapshot"
sources$snapshot_sha256 <- archive_hash
# Keep the release's native schema and basket identifiers.  The longitudinal
# dataset joins like-for-like concepts across those source eras, but each
# observation must continue to identify the exact official schema that
# published it.
source_columns <- c(
  "manifest_version", "dataset_id", "period", "reference_period_start",
  "reference_period_end", "release_date", "source_url", "basket_version",
  "index_base", "expected_press_cells", "schema_version", "storage",
  "snapshot_sha256", "source_capture_sha256"
)
sources <- sources[source_columns]
if (any(sources$expected_press_cells < 1L) ||
    anyDuplicated(paste(sources$dataset_id, sources$period, sep = "\r"))) {
  stop("Longitudinal release index is invalid.", call. = FALSE)
}
utils::write.csv(
  sources, file.path(out, "nbs-longitudinal-release-index.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

# ---- Dimension members --------------------------------------------------

base_dimensions <- read_character_csv(file.path(
  out, "nbs-wages-history-dataset-dimensions.csv"
))
existing_label <- function(dimension, value) {
  candidates <- base_dimensions[
    base_dimensions$dimension == dimension &
      base_dimensions$value == value,
    "label_en"
  ]
  candidates <- candidates[nzchar(candidates)]
  if (length(candidates)) return(candidates[[1L]])
  NULL
}
lookup_label <- function(dimension, value) {
  candidate <- existing_label(dimension, value)
  if (!is.null(candidate)) return(candidate)
  tools::toTitleCase(gsub("_", " ", value, fixed = TRUE))
}

dimension_rows <- list()
add_dimension <- function(dataset_id, dimension, value, label_en, label_zh,
                          valid_from, valid_to, unit_en = "", unit_zh = "",
                          semantics = "") {
  dimension_rows[[length(dimension_rows) + 1L]] <<- data.frame(
    manifest_version = version,
    dataset_id = dataset_id,
    dimension = dimension,
    value = value,
    label_en = label_en,
    label_zh = label_zh,
    route_value = value,
    sort_order = length(dimension_rows) + 1L,
    valid_from = valid_from,
    valid_to = valid_to,
    unit_en = unit_en,
    unit_zh = unit_zh,
    semantics = semantics,
    stringsAsFactors = FALSE
  )
}

for (dimension in c("unit_type", "breakdown", "category", "measure")) {
  values <- unique(wages[[dimension]])
  preferred <- switch(
    dimension,
    unit_type = c("non_private", "private"),
    breakdown = c("overall", "region", "industry"),
    measure = c("average_wage", "nominal_growth_rate"),
    category = c(
      "total", macro_regions,
      sort(setdiff(values, c("total", macro_regions)))
    ),
    sort(values)
  )
  values <- preferred[preferred %in% values]
  for (value in values) {
    member <- wages[wages[[dimension]] == value, , drop = FALSE]
    label <- switch(
      dimension,
      unit_type = c(
        non_private = "Urban non-private units",
        private = "Urban private units"
      )[[value]],
      breakdown = c(
        overall = "National total", region = "Major region",
        industry = "Industry"
      )[[value]],
      measure = c(
        average_wage = "Average annual wage",
        nominal_growth_rate = "Nominal year-on-year growth"
      )[[value]],
      category = if (value == "total") "Total" else {
        region_label <- existing_label("region", value)
        industry_label <- existing_label("industry", value)
        if (!is.null(region_label)) region_label else if (
          !is.null(industry_label)
        ) industry_label else tools::toTitleCase(
          gsub("_", " ", value, fixed = TRUE)
        )
      }
    )
    unit_en <- if (dimension == "measure") {
      if (value == "average_wage") "Yuan" else "Percent"
    } else ""
    unit_zh <- if (dimension == "measure") {
      if (value == "average_wage") "元" else "%"
    } else ""
    add_dimension(
      wage_id, dimension, value, label, label,
      min(member$observation_date), max(member$observation_date),
      unit_en, unit_zh,
      if (dimension == "measure") "single_period" else ""
    )
  }
}

series_labels <- unique(migrant[c("series", "canonical_label", "unit")])
series_labels <- series_labels[order(
  series_labels$canonical_label !=
    "Migrant-worker numbers — Migrant workers, total",
  series_labels$canonical_label
), , drop = FALSE]
for (index in seq_len(nrow(series_labels))) {
  series <- series_labels$series[[index]]
  member <- migrant[migrant$series == series, , drop = FALSE]
  unit_en <- series_labels$unit[[index]]
  unit_zh <- switch(
    unit_en,
    "10,000 persons" = "万人", "Percent" = "%",
    "Percentage points" = "百分点", "Yuan" = "元",
    "Yuan per worker per month" = "元/人/月",
    "Yuan per person per month" = "元/人/月",
    "Months per year" = "月/年", "Days per month" = "天/月",
    "Hours per day" = "小时/天", unit_en
  )
  add_dimension(
    migrant_id, "series", series, series_labels$canonical_label[[index]],
    series_labels$canonical_label[[index]],
    min(member$observation_date), max(member$observation_date),
    unit_en, unit_zh, "single_period"
  )
}

dimensions <- do.call(rbind, dimension_rows)
utils::write.csv(
  dimensions, file.path(out, "nbs-longitudinal-dataset-dimensions.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

# ---- Dataset manifest ---------------------------------------------------

manifest <- data.frame(
  manifest_version = rep(version, 2L),
  dataset_id = c(wage_id, migrant_id),
  release_id = c("cn_nbs_wages", "cn_nbs_migrant_workers"),
  family = c("wages", "migrant_workers"),
  title_en = c(
    "Employment and wages — longitudinal series",
    "Migrant workers — longitudinal series"
  ),
  title_zh = c("就业人员工资—纵向序列", "农民工—纵向序列"),
  description_en = c(
    paste(
      "Continuous official annual wage and growth histories for like-for-like",
      "national, regional and industry concepts across retained classification eras."
    ),
    paste(
      "Longitudinal official indicators reconstructed from repeated observations",
      "in the retained 2012–25 migrant-worker monitoring reports."
    )
  ),
  description_zh = c(
    "跨已保留分类时期、按可比全国、地区和行业概念连接的官方年度工资及增速历史。",
    "依据已保留的2012—2025年农民工监测调查报告中的重复观测整理的官方纵向指标。"
  ),
  frequency = c("A", "A"),
  route = c(
    "retained_longitudinal_series", "retained_longitudinal_series"
  ),
  default_start = c("2010-01-01", "2008-01-01"),
  available_to = c("2025-12-31", "2025-12-31"),
  coverage_en = c(
    paste(
      "Comparable official observations from 2010 to 2025; classification and",
      "2012 coverage breaks remain explicit in row provenance and notes."
    ),
    paste(
      "Like-for-like indicators with at least nine observed years; major",
      "population, age, industry and income series extend through 2025."
    )
  ),
  coverage_zh = c(
    "2010—2025年可比官方观测；分类变化及2012年统计口径变化仍在逐行来源和注释中明确保留。",
    "至少含九个观测年度的同口径指标；主要规模、年龄、行业及收入序列延伸至2025年。"
  ),
  required_dimensions = c(
    "unit_type|breakdown|category|measure", "series"
  ),
  panel_allowed = c("TRUE", "TRUE"),
  expected_rows = c(
    nrow(unique(wages[c("unit_type", "breakdown", "category", "measure")])),
    length(unique(migrant$series))
  ),
  expected_cells = c(nrow(wages), nrow(migrant)),
  aggregation_basis = c("single_period", "single_period"),
  seasonal_adjustment = c("not_applicable", "not_applicable"),
  derived_method = c("none", "none"),
  source_vintage = c(
    "latest_retained_official_observation",
    "latest_retained_official_observation"
  ),
  series_note_en = c(
    paste(
      "Official levels are joined without rebasing or interpolation. The",
      "non-private wage coverage changes from in-post workers to employed",
      "persons in 2012; users should treat that date as a level-series break."
    ),
    paste(
      "Repeated observations are deduplicated by concept and observation year,",
      "retaining the latest official report available in the frozen archive.",
      "Sparse release cells remain available in the native-table archive; no",
      "interpolation or synthetic backcast is used."
    )
  ),
  series_note_zh = c(
    "官方水平值不作重定基或插值。2012年非私营单位工资口径由在岗职工转为就业人员，使用者应将该年视为水平序列断点。",
    "重复观测按指标概念和观测年度去重，并保留冻结档案中最新的官方报告。稀疏发布单元仍可在原始表档案中查阅；未使用插值或合成回溯。"
  ),
  unit_en = c("Yuan or percent", "Official published unit"),
  unit_zh = c("元或百分比", "官方发布单位"),
  source_url = c(
    "https://www.stats.gov.cn/english/PressRelease/202605/t20260518_1963740.html",
    "https://www.stats.gov.cn/zwfwck/sjfb/202604/t20260430_1963472.html"
  ),
  source_note = c(
    paste(
      "Official NBS annual wage releases are retained with exact source URLs",
      "and SHA-256 provenance; duplicate prior-year columns are resolved to",
      "the latest retained official publication."
    ),
    paste(
      "Official NBS migrant-worker reports and exact source hashes are retained;",
      "source-vintage provenance identifies the report selected for each observation."
    )
  ),
  period_policy = c("all_periods", "all_periods"),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest, file.path(out, "nbs-longitudinal-dataset-manifest.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

cat(sprintf(
  paste0(
    "Built %d wage observations across %d exact series and %d migrant-worker ",
    "observations across %d exact series; archive %.1f KiB; SHA-256 %s\n"
  ),
  nrow(wages),
  nrow(unique(wages[c("unit_type", "breakdown", "category", "measure")])),
  nrow(migrant), length(unique(migrant$series)),
  file.info(archive_path)$size / 1024, archive_hash
))
