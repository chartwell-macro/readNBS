# Dataset-scoped loaders for retained press archives. Runtime requests use
# these loaders so a small historical table does not inflate every unrelated
# release archive. The older combined loader remains available to maintenance
# code while public fetch and coverage paths stay lazy.

.nbs_retained_press_cache <- new.env(parent = emptyenv())
.nbs_retained_press_period_index_cache <- new.env(parent = emptyenv())

.nbs_bind_archive_rows <- function(frames) {
  frames <- Filter(function(frame) !is.null(frame) && nrow(frame), frames)
  if (!length(frames)) return(data.frame())
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(frame) {
    for (column in setdiff(columns, names(frame))) frame[[column]] <- ""
    frame[columns]
  })
  do.call(rbind, frames)
}

.nbs_load_retained_press_file <- function(filename, dataset_ids, cache_key) {
  if (exists(cache_key, envir = .nbs_retained_press_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .nbs_retained_press_cache, inherits = FALSE))
  }
  path <- .nbs_extdata_path(filename)
  # `fread(path)` first expands gzip inputs to a temporary file.  The retained
  # housing and core archives are intentionally compact on disk but expand to
  # tens of megabytes, so that path can fail on otherwise healthy low-disk
  # deployments and return a truncated table.  Decompress into memory and let
  # fread parse the resulting text directly: this avoids the temporary copy
  # while retaining the fast parser.
  compressed <- readBin(path, what = "raw", n = file.info(path)$size)
  archive_text <- rawToChar(memDecompress(compressed, type = "gzip"))
  rows <- data.table::fread(
    text = archive_text,
    data.table = FALSE,
    showProgress = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = NULL,
    encoding = "UTF-8"
  )
  rm(compressed, archive_text)
  releases <- .nbs_load_release_index()
  releases <- releases[
    releases$storage == "snapshot" & releases$dataset_id %in% dataset_ids,
    , drop = FALSE
  ]
  expected_hashes <- unique(releases$snapshot_sha256)
  actual_hash <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  required <- c(
    "dataset_id", "release_period", "raw_value", "value", "observation_date",
    "schema_version"
  )
  if (!nrow(rows) || any(!required %in% names(rows)) ||
      any(!rows$dataset_id %in% dataset_ids) || !length(expected_hashes) ||
      !actual_hash %in% expected_hashes ||
      any(!is.finite(suppressWarnings(as.numeric(
        rows$value[nzchar(rows$value)]
      ))))) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("The retained `%s` archive is invalid.", filename)
    )
  }
  rows$value <- as.numeric(rows$value)
  rows$observation_date <- as.Date(ifelse(
    nzchar(rows$observation_date), rows$observation_date, NA_character_
  ))
  for (column in intersect(c("table_index", "row_index", "column_index"), names(rows))) {
    rows[[column]] <- suppressWarnings(as.integer(rows[[column]]))
  }
  assign(cache_key, rows, envir = .nbs_retained_press_cache)
  rows
}

.nbs_load_core_press_observations <- function() {
  .nbs_load_retained_press_file(
    "nbs-press-observations.csv.gz",
    c(
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY",
      "NBS/CN/M/HOUSING_PRICES/70_CITY",
      "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION",
      "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
      "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION",
      "NBS/CN/M/RETAIL_SALES/DETAIL",
      "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
      "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION",
      "NBS/CN/A/EMPLOYMENT_WAGES",
      "NBS/CN/A/MIGRANT_WORKERS",
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
    ),
    "core"
  )
}

.nbs_load_housing_history_observations <- function() {
  .nbs_load_retained_press_file(
    "nbs-housing-history-observations.csv.gz",
    "NBS/CN/M/HOUSING_PRICES/70_CITY", "housing_history"
  )
}

.nbs_load_wage_history_observations <- function() {
  .nbs_load_retained_press_file(
    "nbs-wages-history-observations.csv.gz",
    c("NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", "NBS/CN/A/WAGES/PRIVATE_HISTORY"),
    "wage_history"
  )
}

.nbs_load_longitudinal_observations <- function() {
  .nbs_load_retained_press_file(
    "nbs-longitudinal-observations.csv.gz",
    c(
      "NBS/CN/A/WAGES/LONGITUDINAL",
      "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
    ),
    "longitudinal"
  )
}

.nbs_load_households_legacy_observations <- function() {
  .nbs_load_retained_press_file(
    "nbs-households-legacy-observations.csv.gz",
    c(
      "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
      "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012"
    ),
    "households_legacy"
  )
}

.nbs_retained_press_rows_for_period <- function(
    rows,
    dataset_id,
    period,
    cache_key) {
  if (!nzchar(period)) return(rows)
  index_key <- paste(cache_key, dataset_id, sep = "::")
  if (!exists(
    index_key,
    envir = .nbs_retained_press_period_index_cache,
    inherits = FALSE
  )) {
    relevant <- which(rows$dataset_id == dataset_id)
    index <- split(relevant, rows$release_period[relevant], drop = TRUE)
    assign(
      index_key,
      index,
      envir = .nbs_retained_press_period_index_cache
    )
  }
  index <- get(
    index_key,
    envir = .nbs_retained_press_period_index_cache,
    inherits = FALSE
  )
  selected <- index[[period]]
  if (is.null(selected)) {
    rows[FALSE, , drop = FALSE]
  } else {
    rows[selected, , drop = FALSE]
  }
}

.nbs_press_observations_for_release <- function(dataset_id, period) {
  if (dataset_id %in% c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  )) return(.nbs_retained_press_rows_for_period(
    .nbs_load_longitudinal_observations(), dataset_id, period, "longitudinal"
  ))
  if (dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES" &&
      nzchar(period) && period < "2020-01-D1") {
    return(.nbs_tenday_history_observations_for_release(period))
  }
  if (dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY" && period < "2019-05") {
    return(.nbs_retained_press_rows_for_period(
      .nbs_load_housing_history_observations(), dataset_id, period,
      "housing_history"
    ))
  }
  if (dataset_id %in% c(
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY", "NBS/CN/A/WAGES/PRIVATE_HISTORY"
  )) return(.nbs_retained_press_rows_for_period(
    .nbs_load_wage_history_observations(), dataset_id, period, "wage_history"
  ))
  if (dataset_id %in% c(
    "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
    "NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012"
  )) return(.nbs_retained_press_rows_for_period(
    .nbs_load_households_legacy_observations(), dataset_id, period,
    "households_legacy"
  ))
  if (dataset_id == "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE" && period %in% c("2010", "2011")) {
    return(.nbs_retained_press_rows_for_period(
      .nbs_load_demography_history_observations(), dataset_id, period,
      "demography_history"
    ))
  }
  if (dataset_id == "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024") {
    return(.nbs_retained_press_rows_for_period(
      .nbs_load_migrant_history_observations(), dataset_id, period,
      "migrant_history"
    ))
  }
  if (dataset_id %in% c(
    "NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY",
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019"
  )) return(.nbs_retained_press_rows_for_period(
    .nbs_load_capacity_history_observations(), dataset_id, period,
    "capacity_history"
  ))
  .nbs_retained_press_rows_for_period(
    .nbs_load_core_press_observations(), dataset_id, period, "core"
  )
}

.nbs_press_observations_for_dataset <- function(dataset_id) {
  if (dataset_id %in% c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
  )) return(.nbs_load_longitudinal_observations())
  if (dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES") {
    return(.nbs_bind_archive_rows(list(
      .nbs_load_tenday_history_observations(),
      .nbs_load_core_press_observations()
    )))
  }
  if (dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY") {
    return(.nbs_bind_archive_rows(list(
      .nbs_load_housing_history_observations(),
      .nbs_load_core_press_observations()
    )))
  }
  if (dataset_id == "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE") {
    return(.nbs_bind_archive_rows(list(
      .nbs_load_demography_history_observations(),
      .nbs_load_core_press_observations()
    )))
  }
  .nbs_press_observations_for_release(dataset_id, "")
}
