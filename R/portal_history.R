.nbs_dataset_portal_map_columns <- c(
  "manifest_version", "route_type", "dataset_id", "dimension_key",
  "category_code", "series_id"
)

# The table deliberately records only exact, already verified equivalences.
# It is not a discovery cache: an absent row means that the dataset must retain
# its snapshot route rather than guessing from a similarly named portal leaf.
.nbs_load_dataset_portal_map <- function() {
  if (exists("portal_map", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE)) {
    return(get("portal_map", envir = .nbs_dataset_metadata_cache,
      inherits = FALSE))
  }
  map <- utils::read.csv(
    .nbs_extdata_path("nbs-dataset-portal-map.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  supplements <- c(
    "nbs-price-bindings.csv",
    "nbs-industrial-bindings.csv.gz",
    "nbs-industrial-longitudinal-bindings.csv.gz",
    "nbs-demography-longitudinal-bindings.csv",
    "nbs-key-output-bindings.csv",
    "nbs-retail-bindings.csv"
  )
  for (filename in supplements) {
    map <- rbind(map, utils::read.csv(
      .nbs_extdata_path(filename),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      fileEncoding = "UTF-8"
    ))
  }
  missing <- setdiff(.nbs_dataset_portal_map_columns, names(map))
  if (length(missing)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf(
        "The dataset portal map is missing required columns: %s.",
        paste(missing, collapse = ", ")
      )
    )
  }
  if (any(!map$route_type %in% c("exact", "retail_category"))) {
    .nbs_abort(
      "nbs_manifest_error",
      "The dataset portal map has an unsupported route type."
    )
  }
  known_datasets <- .nbs_load_dataset_manifest()$dataset_id
  if (any(!map$dataset_id %in% known_datasets)) {
    .nbs_abort(
      "nbs_manifest_error",
      "A dataset portal map row refers to an unknown dataset."
    )
  }
  exact <- map[map$route_type == "exact", , drop = FALSE]
  if (any(!nzchar(exact$dimension_key)) || any(!nzchar(exact$series_id)) ||
      anyDuplicated(paste(exact$dataset_id, exact$dimension_key, sep = "\r"))) {
    .nbs_abort(
      "nbs_manifest_error",
      "Exact dataset portal map rows must have unique keys and series IDs."
    )
  }
  retail <- map[map$route_type == "retail_category", , drop = FALSE]
  if (any(!nzchar(retail$category_code)) || any(!nzchar(retail$series_id)) ||
      anyDuplicated(paste(retail$dataset_id, retail$category_code, sep = "\r"))) {
    .nbs_abort(
      "nbs_manifest_error",
      "Retail portal map rows must have unique category codes and stems."
    )
  }
  assign("portal_map", map, envir = .nbs_dataset_metadata_cache)
  map
}

.nbs_dataset_portal_map_series_id <- function(dataset_id, filters) {
  map <- .nbs_load_dataset_portal_map()
  exact <- map[
    map$route_type == "exact" & map$dataset_id == dataset_id,
    , drop = FALSE
  ]
  key <- nbs_dimension_key(filters)
  matched <- exact$series_id[exact$dimension_key == key]
  if (length(matched)) {
    return(matched[[1L]])
  }

  retail <- map[
    map$route_type == "retail_category" & map$dataset_id == dataset_id &
      map$category_code == (filters$category_code %||% ""),
    , drop = FALSE
  ]
  if (!nrow(retail)) {
    return("")
  }
  suffix <- switch(
    paste(filters$period_basis %||% "", filters$measure %||% "", sep = "\r"),
    "current\rlevel" = "CURRENT_LEVEL",
    "current\ryoy_percent" = "CURRENT_YOY_PERCENT",
    "ytd\rlevel" = "YTD_LEVEL",
    "ytd\ryoy_percent" = "YTD_YOY_PERCENT",
    ""
  )
  if (!nzchar(suffix)) {
    return("")
  }
  paste(retail$series_id[[1L]], suffix, sep = "/")
}
