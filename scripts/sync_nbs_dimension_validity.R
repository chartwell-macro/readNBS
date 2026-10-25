#!/usr/bin/env Rscript

# Synchronise the public dimension catalogue with the bundled press archive.
# Existing earlier starts (for example, exact scalar portal histories) are
# preserved. Press-only values are opened or closed according to the releases
# in which they were actually published.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
dimensions_path <- file.path(root, "inst", "extdata", "nbs-dataset-dimensions.csv")
observations_path <- file.path(root, "inst", "extdata", "nbs-press-observations.csv.gz")
index_path <- file.path(root, "inst", "extdata", "nbs-release-index.csv")

dimensions <- read.csv(
  dimensions_path, stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = "", colClasses = "character"
)
observations <- read.csv(
  gzfile(observations_path), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = ""
)
index <- read.csv(
  index_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
)

index_key <- paste(index$dataset_id, index$period, index$source_url, sep = "\r")
observation_key <- paste(
  observations$dataset_id,
  observations$release_period,
  observations$release_source_url,
  sep = "\r"
)
matched_release <- match(observation_key, index_key)
if (anyNA(matched_release)) {
  stop("Every archived observation must match one release-index row.")
}
observations$.reference_start <- index$reference_period_start[matched_release]
observations$.reference_end <- index$reference_period_end[matched_release]
latest_reference <- tapply(
  index$reference_period_start, index$dataset_id, max, na.rm = TRUE
)

for (row in seq_len(nrow(dimensions))) {
  column <- if (dimensions$dimension[[row]] == "category_code") {
    "category"
  } else {
    dimensions$dimension[[row]]
  }
  if (!column %in% names(observations)) next

  selected <- observations[
    observations$dataset_id == dimensions$dataset_id[[row]] &
      !is.na(observations[[column]]) &
      observations[[column]] == dimensions$value[[row]],
    ,
    drop = FALSE
  ]
  if (!nrow(selected)) next

  starts <- selected$.reference_start
  # Wage tables publish the preceding year's level beside the current year.
  # Keep that observation in its actual year rather than the release year.
  if (dimensions$dataset_id[[row]] == "NBS/CN/A/EMPLOYMENT_WAGES") {
    observation_starts <- selected$observation_date
    use_observation <- !is.na(observation_starts) & nzchar(observation_starts)
    starts[use_observation] <- observation_starts[use_observation]
  }
  starts <- starts[!is.na(starts) & nzchar(starts)]
  if (length(starts)) {
    earliest <- min(starts)
    current <- dimensions$valid_from[[row]]
    if (is.na(current) || !nzchar(current) || earliest < current) {
      dimensions$valid_from[[row]] <- earliest
    }
  }

  reference_starts <- selected$.reference_start
  usable <- !is.na(reference_starts) & nzchar(reference_starts)
  if (!any(usable)) next
  latest_value_reference <- max(reference_starts[usable])
  dataset_latest <- unname(latest_reference[dimensions$dataset_id[[row]]])
  if (identical(latest_value_reference, dataset_latest)) {
    dimensions$valid_to[[row]] <- NA_character_
  } else {
    candidates <- selected$.reference_end[
      usable & reference_starts == latest_value_reference
    ]
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    dimensions$valid_to[[row]] <- if (length(candidates)) {
      max(candidates)
    } else {
      latest_value_reference
    }
  }
}

# The regional-property release retained the legacy three-region definition
# through the January-May 2017 cumulative observation. Start the replacement
# four-region definition on the following day so the two schemas never overlap.
property_id <- "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION"
legacy_regions <- dimensions$dataset_id == property_id &
  dimensions$dimension == "region" &
  grepl("_legacy_three_region$", dimensions$value)
current_regions <- dimensions$dataset_id == property_id &
  dimensions$dimension == "region" &
  dimensions$value %in% c("east", "central", "west", "northeast")
legacy_ends <- dimensions$valid_to[legacy_regions]
legacy_ends <- legacy_ends[!is.na(legacy_ends) & nzchar(legacy_ends)]
if (length(legacy_ends) && any(current_regions)) {
  dimensions$valid_from[current_regions] <- as.character(
    as.Date(max(legacy_ends)) + 1L
  )
}

write.csv(
  dimensions, dimensions_path, row.names = FALSE, na = "", quote = TRUE
)
cat(sprintf("Updated %d dimension-catalogue rows.\n", nrow(dimensions)))
