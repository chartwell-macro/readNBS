#' Build a canonical NBS dataset dimension key
#'
#' Dimension names are sorted bytewise, names and values are URL encoded, and
#' pairs are joined with `"|"`. The representation is shared by readNBS and
#' downstream applications so dimensional observations have stable identity.
#'
#' @param filters A named list or named atomic vector of scalar dimension
#'   values.
#' @return One canonical character key.
#' @export
nbs_dimension_key <- function(filters) {
  if (is.null(filters) || !length(filters)) {
    return("")
  }
  if (!is.list(filters)) {
    filters <- as.list(filters)
  }
  names_filters <- names(filters)
  if (is.null(names_filters) || anyNA(names_filters) ||
      any(!nzchar(names_filters)) || anyDuplicated(names_filters)) {
    .nbs_abort(
      "nbs_dimension_error",
      "`filters` must have unique, non-empty dimension names."
    )
  }
  values <- vapply(seq_along(filters), function(index) {
    value <- filters[[index]]
    if (length(value) != 1L || is.na(value) ||
        !is.atomic(value) || is.list(value)) {
      .nbs_abort(
        "nbs_dimension_error",
        sprintf(
          "Dimension `%s` must contain exactly one non-missing scalar.",
          names_filters[[index]]
        )
      )
    }
    as.character(value)
  }, character(1))
  order_index <- order(names_filters, method = "radix")
  paste(
    paste0(
      utils::URLencode(names_filters[order_index], reserved = TRUE),
      "=",
      utils::URLencode(values[order_index], reserved = TRUE)
    ),
    collapse = "|"
  )
}

.nbs_valid_tuple_cache <- new.env(parent = emptyenv())

.nbs_dimension_keys_from_frame <- function(rows, dimensions = names(rows)) {
  if (!nrow(rows)) {
    return(character())
  }
  dimensions <- as.character(dimensions)
  ordered <- order(dimensions, method = "radix")
  encoded_parts <- lapply(ordered, function(index) {
    values <- as.character(rows[[dimensions[[index]]]])
    unique_values <- unique(values)
    encoded_values <- vapply(
      unique_values,
      utils::URLencode,
      character(1L),
      reserved = TRUE,
      USE.NAMES = FALSE
    )
    paste0(
      utils::URLencode(dimensions[[index]], reserved = TRUE),
      "=",
      encoded_values[match(values, unique_values)]
    )
  })
  do.call(paste, c(encoded_parts, list(sep = "|")))
}

#' List valid exact tuples for an NBS dataset
#'
#' Dimension member catalogues are intentionally normalized: a member is
#' listed once even when it appears in several official tables. This function
#' restores the cross-dimension constraints and returns only combinations that
#' are actually published. Applications should use it for dependent selectors
#' rather than presenting a Cartesian product of dimension members.
#'
#' @param dataset_id A stable ID from [nbs_dataset_catalogue()].
#' @return A data frame with one column per required dimension and a canonical
#'   `dimension_key`.
#' @export
nbs_dataset_valid_tuples <- function(dataset_id) {
  dataset_id <- .nbs_scalar_character(dataset_id, "dataset_id")
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  if (!nrow(dataset)) {
    .nbs_abort(
      "nbs_unknown_dataset_error",
      sprintf("Unknown NBS dataset `%s`.", dataset_id),
      dataset_id = dataset_id
    )
  }
  cache_key <- paste(dataset$manifest_version[[1L]], dataset_id, sep = "\r")
  if (exists(cache_key, envir = .nbs_valid_tuple_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .nbs_valid_tuple_cache, inherits = FALSE))
  }

  required <- strsplit(
    dataset$required_dimensions[[1L]], "|", fixed = TRUE
  )[[1L]]
  if (identical(dataset$route[[1L]], "yearbook_snapshot")) {
    archive <- .nbs_load_yearbook_observations()
    rows <- archive[archive$dataset_id == dataset_id, , drop = FALSE]
    if (!nrow(rows) || any(!required %in% names(rows))) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf("Yearbook dataset `%s` has no exact archive tuples.", dataset_id)
      )
    }
    tuples <- unique(rows[required])
    tuples$dimension_key <- .nbs_dimension_keys_from_frame(tuples, required)
    tuples <- tuples[order(tuples$dimension_key), , drop = FALSE]
    rownames(tuples) <- NULL
    assign(cache_key, tuples, envir = .nbs_valid_tuple_cache)
    return(tuples)
  }
  if (dataset_id %in% c(
    "NBS/CN/A/WAGES/LONGITUDINAL",
    "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL",
    "NBS/CN/A/WAGES/NON_PRIVATE_HISTORY",
    "NBS/CN/A/WAGES/PRIVATE_HISTORY",
    "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024",
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
  )) {
    archive <- .nbs_press_observations_for_dataset(dataset_id)
    rows <- archive[archive$dataset_id == dataset_id, , drop = FALSE]
    if (!nrow(rows) || any(!required %in% names(rows))) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf("Dataset `%s` has no exact retained archive tuples.", dataset_id)
      )
    }
    tuples <- unique(rows[required])
    tuples$dimension_key <- .nbs_dimension_keys_from_frame(tuples, required)
    if (dataset_id %in% c(
      "NBS/CN/A/WAGES/LONGITUDINAL",
      "NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL"
    )) {
      members <- nbs_dataset_dimensions(dataset_id, "en")
      sort_columns <- lapply(required, function(dimension) {
        dimension_members <- members[
          members$dimension == dimension, c("value", "sort_order"),
          drop = FALSE
        ]
        dimension_members$sort_order[match(
          tuples[[dimension]], dimension_members$value
        )]
      })
      tuples <- tuples[do.call(order, c(sort_columns, list(
        tuples$dimension_key, method = "radix"
      ))), , drop = FALSE]
    } else {
      tuples <- tuples[order(tuples$dimension_key), , drop = FALSE]
    }
    rownames(tuples) <- NULL
    assign(cache_key, tuples, envir = .nbs_valid_tuple_cache)
    return(tuples)
  }
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  members <- lapply(required, function(name) {
    values <- dimensions$value[dimensions$dimension == name]
    if (!length(values)) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf(
          "Dataset `%s` has no members for required dimension `%s`.",
          dataset_id, name
        )
      )
    }
    values
  })
  names(members) <- required
  tuples <- do.call(
    expand.grid,
    c(members, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )
  valid <- rep(TRUE, nrow(tuples))

  if (dataset_id ==
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE") {
    ytd_measures <- c(
      "business_revenue_level", "business_revenue_yoy",
      "operating_cost_level", "operating_cost_yoy",
      "total_profit_level", "total_profit_yoy",
      "profit_margin", "cost_per_100_revenue",
      "expenses_per_100_revenue"
    )
    end_measures <- c(
      "revenue_per_100_assets", "revenue_per_person",
      "asset_liability_ratio", "finished_goods_turnover_days",
      "receivables_collection_days"
    )
    valid <- (
      tuples$measure %in% ytd_measures & tuples$period_basis == "ytd"
    ) | (
      tuples$measure %in% end_measures &
        tuples$period_basis == "end_period"
    )
  } else if (dataset_id == "NBS/CN/M/HOUSING_PRICES/70_CITY") {
    valid <- (
      tuples$index_base == "all_official_bases" &
        tuples$comparison %in% c("mom", "yoy", "ytd_avg_yoy")
    ) | (
      tuples$index_base == "2010" &
        tuples$comparison %in% c("mom", "yoy", "fixed_base")
    ) | (
      tuples$index_base == "2015" &
        tuples$comparison %in% c("mom", "yoy", "fixed_base")
    ) | (
      tuples$index_base == "2020" &
        tuples$comparison %in% c("mom", "yoy", "fixed_base", "ytd_avg_yoy")
    ) | (
      tuples$index_base == "2025" &
        tuples$comparison %in% c("mom", "yoy", "ytd_avg_yoy")
    )
  } else if (dataset_id == "NBS/CN/M/RETAIL_SALES/DETAIL") {
    ordinary <- tuples$measure %in% c("level", "yoy_percent") &
      tuples$period_basis %in% c("current", "ytd") &
      !(
        tuples$category_code ==
          "of_which_online_retail_sales_of_goods" &
          tuples$period_basis == "current"
      )
    official_sa <- tuples$measure == "mom_percent_official_sa" &
      tuples$category_code == "total_retail_sales_of_consumer_goods" &
      tuples$period_basis == "current"
    valid <- ordinary | official_sa
  } else if (dataset_id == "NBS/CN/A/EMPLOYMENT_WAGES") {
    entity_count <- (
      tuples$region != "not_applicable"
    ) + (
      tuples$industry != "not_applicable"
    ) + (
      tuples$registration != "not_applicable"
    )
    simple_measures <- c(
      "current_year_average_wage", "prior_year_average_wage",
      "nominal_growth_rate"
    )
    non_private <- tuples$unit_type == "non_private" &
      entity_count == 1L &
      tuples$occupation == "not_applicable" &
      tuples$measure %in% simple_measures
    private <- tuples$unit_type == "private" &
      entity_count == 1L &
      tuples$occupation == "not_applicable" &
      tuples$measure %in% simple_measures &
      tuples$registration == "not_applicable" &
      tuples$industry !=
        "public_management_social_security_and_social_organizations"
    unavailable_above_designated <- tuples$industry %in% c(
      "agriculture_forestry_animal_husbandry_and_fishery",
      "finance",
      "public_management_social_security_and_social_organizations"
    )
    above_designated <- tuples$unit_type == "above_designated" &
      entity_count == 1L &
      tuples$occupation != "not_applicable" &
      tuples$measure == "occupation_average_wage" &
      !unavailable_above_designated
    valid <- non_private | private | above_designated
  } else if (dataset_id ==
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES") {
    direction <- c(
      "increased_product_count", "decreased_product_count",
      "flat_product_count"
    )
    valid <- (
      tuples$product_code == "summary" &
      tuples$measure %in% direction
    ) | (
      tuples$product_code != "summary" &
      !tuples$measure %in% direction
    )
  }

  # A portal UUID panel is admitted only through exact, versioned bindings.
  # This is deliberately generic: price datasets (CPI now, PPI later) can
  # share the same machinery without a dataset-specific compatibility branch.
  if (identical(dataset$route[[1L]], "portal_uuid_panel")) {
    bindings <- .nbs_load_dataset_portal_map()
    bindings <- bindings[
      bindings$route_type == "exact" &
        bindings$dataset_id == dataset_id,
      , drop = FALSE
    ]
    if (!nrow(bindings)) {
      .nbs_abort(
        "nbs_manifest_error",
        sprintf(
          "Portal UUID panel `%s` has no exact dimension bindings.",
          dataset_id
        )
      )
    }
    tuple_keys <- .nbs_dimension_keys_from_frame(tuples, required)
    valid <- valid & tuple_keys %in% bindings$dimension_key
  }

  tuples <- tuples[valid, , drop = FALSE]
  tuples$dimension_key <- .nbs_dimension_keys_from_frame(tuples, required)
  if (anyDuplicated(tuples$dimension_key)) {
    .nbs_abort(
      "nbs_manifest_error",
      sprintf("Dataset `%s` generated duplicate valid tuples.", dataset_id)
    )
  }
  rownames(tuples) <- NULL
  assign(cache_key, tuples, envir = .nbs_valid_tuple_cache)
  tuples
}

.nbs_dataset_observation_matches <- function(
    dataset_id,
    dimension,
    requested,
    observed) {
  observed <- as.character(observed)
  if (identical(dataset_id, "NBS/CN/M/HOUSING_PRICES/70_CITY") &&
      identical(dimension, "index_base") &&
      identical(requested, "all_official_bases")) {
    return(!is.na(observed) & nzchar(observed))
  }
  !is.na(observed) & observed == requested
}
