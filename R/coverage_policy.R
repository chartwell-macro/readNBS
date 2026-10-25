.nbs_coverage_policy_cache <- new.env(parent = emptyenv())
.nbs_coverage_schema_cache <- new.env(parent = emptyenv())
.nbs_historical_coverage_cache <- new.env(parent = emptyenv())

.nbs_coverage_policy_columns <- c(
  "policy_version", "policy_id", "entity_type", "match_type", "identity",
  "dimension_filters", "classification_id", "required_from", "reason",
  "schema_era", "authority"
)

.nbs_load_coverage_policy <- function() {
  if (exists("policy", envir = .nbs_coverage_policy_cache, inherits = FALSE)) {
    return(get("policy", envir = .nbs_coverage_policy_cache, inherits = FALSE))
  }
  policy <- utils::read.csv(
    .nbs_extdata_path("nbs-coverage-policy.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  missing <- setdiff(.nbs_coverage_policy_columns, names(policy))
  if (length(missing) || !nrow(policy)) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      "The bundled NBS historical-coverage policy is malformed."
    )
  }
  if (anyDuplicated(policy$policy_id) ||
      any(!policy$entity_type %in% c("scalar", "dataset")) ||
      any(!policy$match_type %in% c("exact", "prefix", "regex")) ||
      any(!nzchar(policy$identity)) || any(!nzchar(policy$reason)) ||
      any(!nzchar(policy$schema_era)) || any(!nzchar(policy$authority)) ||
      anyNA(suppressWarnings(as.Date(policy$required_from)))) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      "The NBS historical-coverage policy contains invalid values."
    )
  }
  for (index in which(policy$match_type == "regex")) {
    valid <- tryCatch({
      grepl(policy$identity[[index]], "", perl = TRUE)
      TRUE
    }, error = function(error) FALSE)
    if (!isTRUE(valid)) {
      .nbs_abort(
        "nbs_coverage_policy_error",
        sprintf("Coverage-policy regex `%s` is invalid.", policy$policy_id[[index]])
      )
    }
  }
  assign("policy", policy, envir = .nbs_coverage_policy_cache)
  policy
}

.nbs_parse_coverage_filters <- function(value) {
  if (!nzchar(value)) return(list())
  pairs <- strsplit(value, "|", fixed = TRUE)[[1L]]
  result <- lapply(pairs, function(pair) {
    parts <- strsplit(pair, "=", fixed = TRUE)[[1L]]
    if (length(parts) != 2L || !nzchar(parts[[1L]]) || !nzchar(parts[[2L]])) {
      .nbs_abort(
        "nbs_coverage_policy_error",
        "Coverage-policy dimension filters must be `name=value` pairs."
      )
    }
    parts[[2L]]
  })
  names(result) <- vapply(strsplit(pairs, "=", fixed = TRUE), `[[`, character(1), 1L)
  if (anyDuplicated(names(result))) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      "Coverage-policy dimension filters must not repeat a dimension."
    )
  }
  result
}

.nbs_coverage_policy_match <- function(entity_type, identity, filters = list(),
                                       classification_id = "") {
  policy <- .nbs_load_coverage_policy()
  candidates <- policy[policy$entity_type == entity_type, , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  matches <- vapply(seq_len(nrow(candidates)), function(index) {
    row <- candidates[index, , drop = FALSE]
    identity_match <- switch(row$match_type[[1L]],
      exact = identical(identity, row$identity[[1L]]),
      prefix = startsWith(identity, row$identity[[1L]]),
      regex = isTRUE(grepl(row$identity[[1L]], identity, perl = TRUE))
    )
    if (!identity_match) return(FALSE)
    required_filters <- .nbs_parse_coverage_filters(row$dimension_filters[[1L]])
    filters_match <- all(vapply(names(required_filters), function(name) {
      identical(as.character(filters[[name]] %||% ""), required_filters[[name]])
    }, logical(1)))
    classification_match <- !nzchar(row$classification_id[[1L]]) ||
      identical(classification_id, row$classification_id[[1L]])
    filters_match && classification_match
  }, logical(1))
  candidates <- candidates[matches, , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  specificity <- nchar(candidates$identity) +
    1000L * (candidates$match_type == "exact") +
    100L * vapply(
      candidates$dimension_filters,
      function(value) length(.nbs_parse_coverage_filters(value)),
      integer(1)
    ) +
    10L * nzchar(candidates$classification_id)
  candidates[which.max(specificity), , drop = FALSE]
}

.nbs_scalar_schema_eras <- function(series_ids) {
  manifest <- .nbs_load_manifest()
  direct_ids <- unique(manifest$series_id)
  group <- factor(manifest$series_id, levels = direct_ids)
  starts <- as.numeric(as.Date(manifest$slice_start))
  earliest <- unname(tapply(starts, group, min))
  first_slice <- starts == earliest[as.integer(group)]
  classifications <- split(
    manifest$classification_id[first_slice],
    factor(manifest$series_id[first_slice], levels = direct_ids)
  )
  direct_schema <- vapply(
    classifications,
    function(values) paste(unique(values), collapse = "|"),
    character(1)
  )
  matched <- match(series_ids, direct_ids)
  result <- rep("press_revision_table", length(series_ids))
  result[!is.na(matched)] <- unname(direct_schema[matched[!is.na(matched)]])
  result
}

.nbs_scalar_schema_era <- function(series_id, available_from) {
  .nbs_scalar_schema_eras(series_id)[[1L]]
}

.nbs_dataset_snapshot_schema <- function(dataset_id, filters) {
  cache_key <- paste("schema", dataset_id, sep = "\r")
  if (!exists(cache_key, envir = .nbs_coverage_schema_cache, inherits = FALSE)) {
    observations <- .nbs_press_observations_for_dataset(dataset_id)
    manifest <- .nbs_load_dataset_manifest()
    dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
    required <- strsplit(dataset$required_dimensions[[1L]], "|", fixed = TRUE)[[1L]]
    columns <- sub("^category_code$", "category", required)
    rows <- observations[observations$dataset_id == dataset_id, , drop = FALSE]
    releases <- .nbs_load_release_index()
    releases <- releases[releases$dataset_id == dataset_id, , drop = FALSE]
    if (!nrow(rows) || !nrow(releases)) {
      schema <- character()
    } else {
      ordered <- order(required, method = "radix")
      parts <- lapply(ordered, function(index) {
        paste0(
          utils::URLencode(required[[index]], reserved = TRUE), "=",
          utils::URLencode(as.character(rows[[columns[[index]]]]), reserved = TRUE)
        )
      })
      keys <- do.call(paste, c(parts, list(sep = "|")))
      matched <- match(rows$release_period, releases$period)
      starts <- as.numeric(as.Date(releases$reference_period_start[matched]))
      earliest <- tapply(starts, keys, min)
      first_rows <- starts == unname(earliest[keys])
      schema <- vapply(split(rows$schema_version[first_rows], keys[first_rows]), function(values) {
        values <- unique(values)
        paste(values, collapse = "|")
      }, character(1))
    }
    assign(cache_key, schema, envir = .nbs_coverage_schema_cache)
  }
  schema <- get(cache_key, envir = .nbs_coverage_schema_cache, inherits = FALSE)
  key <- nbs_dimension_key(filters)
  if (key %in% names(schema)) return(unname(schema[[key]]))
  "manifest_default"
}

.nbs_dataset_tuple_coverage <- function(dataset_id, filters) {
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  portal_id <- if (grepl("portal_uuid", dataset$route[[1L]], fixed = TRUE)) {
    .nbs_dataset_portal_series_id(dataset_id, filters)
  } else {
    ""
  }
  if (nzchar(portal_id)) {
    catalogue <- nbs_catalogue("en")
    scalar <- catalogue[catalogue$series_id == portal_id, , drop = FALSE]
    coverage <- list(
      available_from = scalar$available_from[[1L]],
      available_to = scalar$available_to[[1L]],
      schema_era = .nbs_scalar_schema_era(portal_id, scalar$available_from[[1L]])
    )
    if (identical(dataset_id, "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION")) {
      snapshot <- .nbs_dataset_snapshot_coverage(dataset_id, filters)
      coverage$available_from <- min(coverage$available_from, snapshot$available_from)
      coverage$available_to <- if (is.na(coverage$available_to) || is.na(snapshot$available_to)) {
        as.Date(NA)
      } else {
        max(coverage$available_to, snapshot$available_to)
      }
      coverage$schema_era <- paste(
        coverage$schema_era, .nbs_dataset_snapshot_schema(dataset_id, filters),
        sep = "|"
      )
    }
    return(coverage)
  }
  if ("series_id" %in% names(filters)) {
    return(list(
      available_from = as.Date(dataset$default_start[[1L]]),
      available_to = as.Date(if (nzchar(dataset$available_to[[1L]])) dataset$available_to[[1L]] else NA),
      schema_era = "manifest_default"
    ))
  }
  snapshot <- .nbs_dataset_snapshot_coverage(dataset_id, filters)
  list(
    available_from = snapshot$available_from,
    available_to = snapshot$available_to,
    schema_era = .nbs_dataset_snapshot_schema(dataset_id, filters)
  )
}

.nbs_dataset_portal_ids <- function(dataset_id, tuples, fields, scalar_ids) {
  result <- rep("", nrow(tuples))
  if (identical(fields, "series_id")) {
    result <- as.character(tuples$series_id)
  } else {
    map <- .nbs_load_dataset_portal_map()
    exact <- map[
      map$route_type == "exact" & map$dataset_id == dataset_id,
      , drop = FALSE
    ]
    if (nrow(exact)) {
      matched <- match(tuples$dimension_key, exact$dimension_key)
      found <- !is.na(matched)
      result[found] <- exact$series_id[matched[found]]
    }
    missing <- which(!nzchar(result))
    if (length(missing)) {
      result[missing] <- vapply(missing, function(index) {
        filters <- as.list(tuples[index, fields, drop = FALSE])
        .nbs_dataset_portal_series_id(dataset_id, filters)
      }, character(1))
    }
  }
  result[!result %in% scalar_ids] <- ""
  result
}

.nbs_dataset_snapshot_coverages <- function(dataset_id, tuples) {
  manifest <- .nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  cache_key <- paste("coverage", dataset_id, sep = "\r")
  if (!exists(cache_key, envir = .nbs_coverage_schema_cache, inherits = FALSE)) {
    if (identical(dataset$route[[1L]], "yearbook_snapshot")) {
      observations <- .nbs_load_yearbook_observations()
      releases <- .nbs_load_yearbook_release_index()
      schema_column <- "schema_era"
    } else {
      observations <- .nbs_press_observations_for_dataset(dataset_id)
      releases <- .nbs_load_release_index()
      schema_column <- "schema_version"
    }
    rows <- observations[observations$dataset_id == dataset_id, , drop = FALSE]
    required <- strsplit(dataset$required_dimensions[[1L]], "|", fixed = TRUE)[[1L]]
    columns <- sub("^category_code$", "category", required)
    if (!nrow(rows) || any(!columns %in% names(rows)) ||
        !schema_column %in% names(rows)) {
      bundle <- list(
        from = numeric(), to = numeric(), schema = character(),
        virtual_from = numeric(), virtual_to = numeric(),
        virtual_schema = character()
      )
    } else {
      ordered <- order(required, method = "radix")
      key_part <- function(values) {
        values <- as.character(values)
        paste0(nchar(values, type = "bytes"), ":", values)
      }
      keys <- do.call(paste, c(
        lapply(columns[ordered], function(column) key_part(rows[[column]])),
        list(sep = "\r")
      ))
      releases <- releases[releases$dataset_id == dataset_id, , drop = FALSE]
      release_match <- match(rows$release_period, releases$period)
      usable <- !is.na(release_match)
      rows <- rows[usable, , drop = FALSE]
      keys <- keys[usable]
      release_match <- release_match[usable]
      if (!length(keys)) {
        bundle <- list(
          from = numeric(), to = numeric(), schema = character(),
          virtual_from = numeric(), virtual_to = numeric(),
          virtual_schema = character()
        )
      } else {
        starts <- as.numeric(as.Date(
          releases$reference_period_start[release_match]
        ))
        ends <- as.numeric(as.Date(
          releases$reference_period_end[release_match]
        ))
        observation_dates <- as.numeric(rows$observation_date)
        has_dates <- !is.na(observation_dates)
        starts[has_dates] <- pmin(
          starts[has_dates], observation_dates[has_dates]
        )
        ends[has_dates] <- pmax(ends[has_dates], observation_dates[has_dates])
        first_order <- order(keys, starts, method = "radix")
        first <- first_order[!duplicated(keys[first_order])]
        last_order <- order(keys, -ends, method = "radix")
        last <- last_order[!duplicated(keys[last_order])]
        first_schema <- vapply(
          split(rows[[schema_column]][first], keys[first]),
          function(values) paste(unique(values), collapse = "|"),
          character(1)
        )
        bundle <- list(
          from = stats::setNames(starts[first], keys[first]),
          to = stats::setNames(ends[last], keys[last]),
          schema = first_schema,
          virtual_from = numeric(), virtual_to = numeric(),
          virtual_schema = character()
        )
        if (identical(dataset_id, "NBS/CN/M/HOUSING_PRICES/70_CITY") &&
            "index_base" %in% required) {
          virtual_required <- setdiff(required, "index_base")
          virtual_ordered <- order(virtual_required, method = "radix")
          virtual_columns <- sub(
            "^category_code$", "category", virtual_required
          )
          virtual_keys <- do.call(paste, c(
            lapply(
              virtual_columns[virtual_ordered],
              function(column) key_part(rows[[column]])
            ),
            list(sep = "\r")
          ))
          virtual_first_order <- order(
            virtual_keys, starts, method = "radix"
          )
          virtual_first <- virtual_first_order[
            !duplicated(virtual_keys[virtual_first_order])
          ]
          virtual_last_order <- order(
            virtual_keys, -ends, method = "radix"
          )
          virtual_last <- virtual_last_order[
            !duplicated(virtual_keys[virtual_last_order])
          ]
          virtual_schema <- vapply(
            split(
              rows[[schema_column]],
              virtual_keys
            ),
            function(values) paste(unique(values), collapse = "|"),
            character(1)
          )
          bundle$virtual_from <- stats::setNames(
            starts[virtual_first], virtual_keys[virtual_first]
          )
          bundle$virtual_to <- stats::setNames(
            ends[virtual_last], virtual_keys[virtual_last]
          )
          bundle$virtual_schema <- virtual_schema
        }
      }
    }
    assign(cache_key, bundle, envir = .nbs_coverage_schema_cache)
  }
  coverage <- get(cache_key, envir = .nbs_coverage_schema_cache, inherits = FALSE)
  required <- strsplit(dataset$required_dimensions[[1L]], "|", fixed = TRUE)[[1L]]
  ordered <- order(required, method = "radix")
  key_part <- function(values) {
    values <- as.character(values)
    paste0(nchar(values, type = "bytes"), ":", values)
  }
  tuple_keys <- do.call(paste, c(
    lapply(required[ordered], function(column) key_part(tuples[[column]])),
    list(sep = "\r")
  ))
  matched <- match(tuple_keys, names(coverage$from))
  virtual <- identical(
    dataset_id, "NBS/CN/M/HOUSING_PRICES/70_CITY"
  ) & tuples$index_base == "all_official_bases"
  if (any(virtual)) {
    virtual_required <- setdiff(required, "index_base")
    virtual_ordered <- order(virtual_required, method = "radix")
    virtual_keys <- do.call(paste, c(
      lapply(
        virtual_required[virtual_ordered],
        function(column) key_part(tuples[[column]])
      ),
      list(sep = "\r")
    ))
    virtual_matched <- match(
      virtual_keys[virtual], names(coverage$virtual_from)
    )
    virtual_rows <- which(virtual)
    has_virtual <- !is.na(virtual_matched)
    if (any(has_virtual)) {
      rows <- virtual_rows[has_virtual]
      matched[rows] <- NA_integer_
    }
  } else {
    virtual_rows <- integer()
    virtual_matched <- integer()
    has_virtual <- logical()
  }
  fallback_from <- as.Date(dataset$default_start[[1L]])
  fallback_to <- as.Date(if (nzchar(dataset$available_to[[1L]])) {
    dataset$available_to[[1L]]
  } else {
    NA_character_
  })
  available_from <- as.Date(ifelse(
      is.na(matched), fallback_from,
      unname(coverage$from[matched])
    ), origin = "1970-01-01")
  available_to <- as.Date(ifelse(
      is.na(matched), fallback_to,
      unname(coverage$to[matched])
    ), origin = "1970-01-01")
  schema_era <- ifelse(
      is.na(matched), "manifest_default", unname(coverage$schema[tuple_keys])
    )
  if (length(virtual_rows) && any(has_virtual)) {
    rows <- virtual_rows[has_virtual]
    positions <- virtual_matched[has_virtual]
    available_from[rows] <- as.Date(
      unname(coverage$virtual_from[positions]), origin = "1970-01-01"
    )
    available_to[rows] <- as.Date(
      unname(coverage$virtual_to[positions]), origin = "1970-01-01"
    )
    schema_era[rows] <- unname(coverage$virtual_schema[
      names(coverage$virtual_from)[positions]
    ])
  }
  data.frame(
    available_from = available_from,
    available_to = available_to,
    schema_era = schema_era,
    stringsAsFactors = FALSE
  )
}

.nbs_coverage_row <- function(entity_type, identity, dataset_id = "",
                              dimension_key = "", filters = list(), frequency,
                              available_from, available_to, schema_era,
                              period_policy = "all_periods",
                              portal_series_id = "") {
  baseline <- .nbs_historical_baseline(frequency, period_policy)
  policy <- .nbs_coverage_policy_match(
    entity_type, identity, filters = filters, classification_id = schema_era
  )
  policy_required <- if (is.null(policy)) baseline else {
    as.Date(policy$required_from[[1L]])
  }
  portal_id <- if (nzchar(portal_series_id)) portal_series_id else {
    if (identical(entity_type, "scalar")) identity else ""
  }
  portal <- .nbs_portal_inception(portal_id)
  portal_required <- if (is.null(portal) || is.na(portal$required_from[[1L]])) {
    as.Date(NA)
  } else {
    portal$required_from[[1L]]
  }
  use_portal <- !is.na(portal_required) && portal_required > policy_required
  required_from <- if (use_portal) portal_required else policy_required
  covered <- !is.na(available_from) && available_from <= required_from
  start_reason <- if (use_portal) "official_portal_uuid_daterange" else {
    if (is.null(policy)) "baseline_2010_requirement" else policy$reason[[1L]]
  }
  policy_id <- if (use_portal) "portal-uuid-inception-index" else {
    if (is.null(policy)) "" else policy$policy_id[[1L]]
  }
  authority <- if (use_portal) portal$authority[[1L]] else {
    if (is.null(policy)) "" else policy$authority[[1L]]
  }
  data.frame(
    entity_type = entity_type,
    identity = identity,
    dataset_id = dataset_id,
    dimension_key = dimension_key,
    frequency = frequency,
    available_from = as.Date(available_from),
    available_to = as.Date(available_to),
    required_from = required_from,
    status = if (covered) "covered" else "unexplained_post_2010_start",
    start_reason = start_reason,
    schema_era = schema_era,
    policy_id = policy_id,
    authority = authority,
    stringsAsFactors = FALSE
  )
}

.nbs_historical_baseline <- function(frequency, period_policy) {
  result <- rep(as.Date("2010-01-01"), length(period_policy))
  monthly <- frequency == "M"
  february <- period_policy %in% c(
    "january_not_reported_february_first_period",
    "ytd_january_not_reported_february_combined",
    "mixed_jan_feb_combined_then_monthly",
    "omit_january"
  )
  march <- period_policy %in% c(
    "current_january_february_not_separately_reported",
    "omit_january_february"
  )
  result[monthly & february] <- as.Date("2010-02-01")
  result[monthly & march] <- as.Date("2010-03-01")
  result
}

.nbs_uniform_dataset_coverage_rows <- function(dataset_id, tuples, frequency,
                                                coverages, period_policy,
                                                portal_ids = rep("", nrow(tuples))) {
  policy_table <- .nbs_load_coverage_policy()
  candidates <- policy_table[
    policy_table$entity_type == "dataset" &
      vapply(seq_len(nrow(policy_table)), function(index) {
        row <- policy_table[index, , drop = FALSE]
        switch(row$match_type[[1L]],
          exact = identical(dataset_id, row$identity[[1L]]),
          prefix = startsWith(dataset_id, row$identity[[1L]]),
          regex = isTRUE(grepl(row$identity[[1L]], dataset_id, perl = TRUE))
        )
      }, logical(1)), , drop = FALSE
  ]
  if (nrow(candidates) && any(nzchar(candidates$dimension_filters) |
      nzchar(candidates$classification_id))) {
    return(NULL)
  }
  policy <- .nbs_coverage_policy_match("dataset", dataset_id)
  tuple_policy <- if ("period_policy" %in% names(coverages)) {
    coverages$period_policy
  } else {
    rep(period_policy, nrow(tuples))
  }
  required_from <- if (is.null(policy)) {
    .nbs_historical_baseline(frequency, tuple_policy)
  } else {
    rep(as.Date(policy$required_from[[1L]]), nrow(tuples))
  }
  portal <- .nbs_portal_inception(portal_ids)
  use_portal <- !is.na(portal$required_from) &
    portal$required_from > required_from
  # Capacity is a deliberate hybrid: the exact GB/T 4754-2017 leaves are
  # retained from their 2018 Q2 press-table inception, before the equivalent
  # portal UUIDs begin in 2021.  The later transport UUID must not erase that
  # independently pinned first-release boundary.
  if (identical(dataset_id, "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION")) {
    use_portal[] <- FALSE
  }
  required_from[use_portal] <- portal$required_from[use_portal]
  start_reason <- rep(if (is.null(policy)) {
    "baseline_2010_requirement"
  } else {
    policy$reason[[1L]]
  }, nrow(tuples))
  start_reason[use_portal] <- "official_portal_uuid_daterange"
  policy_id <- rep(if (is.null(policy)) "" else policy$policy_id[[1L]], nrow(tuples))
  policy_id[use_portal] <- "portal-uuid-inception-index"
  authority <- rep(if (is.null(policy)) "" else policy$authority[[1L]], nrow(tuples))
  authority[use_portal] <- portal$authority[use_portal]
  data.frame(
    entity_type = "dataset",
    identity = dataset_id,
    dataset_id = dataset_id,
    dimension_key = tuples$dimension_key,
    frequency = frequency,
    available_from = coverages$available_from,
    available_to = coverages$available_to,
    required_from = required_from,
    status = ifelse(coverages$available_from <= required_from,
      "covered", "unexplained_post_2010_start"
    ),
    start_reason = start_reason,
    schema_era = coverages$schema_era,
    policy_id = policy_id,
    authority = authority,
    stringsAsFactors = FALSE
  )
}

.nbs_tenday_dataset_coverage_rows <- function(
    dataset_id, tuples, frequency, coverages) {
  if (!identical(dataset_id, "NBS/CN/10D/PRODUCTION_MARKET_PRICES")) {
    return(NULL)
  }
  policy <- .nbs_load_tenday_tuple_inceptions()
  matched <- match(tuples$dimension_key, policy$dimension_key)
  if (anyNA(matched) || anyDuplicated(matched)) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      "The ten-day dataset tuples do not match their exact inception policy."
    )
  }
  required_from <- policy$required_from[matched]
  data.frame(
    entity_type = "dataset", identity = dataset_id, dataset_id = dataset_id,
    dimension_key = tuples$dimension_key, frequency = frequency,
    available_from = coverages$available_from,
    available_to = coverages$available_to,
    required_from = required_from,
    status = ifelse(
      !is.na(coverages$available_from) &
        coverages$available_from <= required_from,
      "covered", "unexplained_post_2010_start"
    ),
    start_reason = policy$reason[matched],
    schema_era = coverages$schema_era,
    policy_id = "tenday-exact-tuple-inception-index",
    authority = policy$authority[matched],
    stringsAsFactors = FALSE
  )
}

#' Audit bundled NBS historical coverage
#'
#' Audits every scalar semantic series and every exact dimensional tuple against
#' the bundled 2010 coverage policy. A post-2010 start is accepted only when a
#' specific policy row records an authoritative later inception (or a named,
#' versioned classification start); the entity must still reach that stated
#' inception date. The function is deterministic and never calls the network.
#'
#' @return A data frame with one row per scalar series or exact dataset tuple.
#'   `status == "covered"` is the completion condition.
#' @export
nbs_historical_coverage <- function() {
  if (exists("audit", envir = .nbs_historical_coverage_cache, inherits = FALSE)) {
    return(get("audit", envir = .nbs_historical_coverage_cache, inherits = FALSE))
  }
  scalar_catalogue <- nbs_catalogue("en")
  scalar_schema <- .nbs_scalar_schema_eras(scalar_catalogue$series_id)
  scalar_rows <- lapply(seq_len(nrow(scalar_catalogue)), function(index) {
    row <- scalar_catalogue[index, , drop = FALSE]
    .nbs_coverage_row(
      "scalar", row$series_id[[1L]], frequency = row$frequency[[1L]],
      available_from = row$available_from[[1L]], available_to = row$available_to[[1L]],
      schema_era = scalar_schema[[index]], period_policy = row$period_policy[[1L]],
      portal_series_id = row$series_id[[1L]]
    )
  })
  datasets <- nbs_dataset_catalogue("en")
  dataset_rows <- unlist(lapply(seq_len(nrow(datasets)), function(index) {
    dataset_id <- datasets$dataset_id[[index]]
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    fields <- setdiff(names(tuples), "dimension_key")
    manifest <- .nbs_load_dataset_manifest()
    dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
    portal_backed <- grepl("portal_uuid", dataset$route[[1L]], fixed = TRUE)
    if (portal_backed) {
      portal_ids <- .nbs_dataset_portal_ids(
        dataset_id, tuples, fields, scalar_catalogue$series_id
      )
      scalar_match <- match(portal_ids, scalar_catalogue$series_id)
      matched <- nzchar(portal_ids) & !is.na(scalar_match)
      coverages <- data.frame(
        available_from = rep(as.Date(NA), nrow(tuples)),
        available_to = rep(as.Date(NA), nrow(tuples)),
        schema_era = rep("", nrow(tuples)),
        period_policy = rep(datasets$period_policy[[index]], nrow(tuples)),
        stringsAsFactors = FALSE
      )
      coverages$available_from[matched] <-
        scalar_catalogue$available_from[scalar_match[matched]]
      coverages$available_to[matched] <-
        scalar_catalogue$available_to[scalar_match[matched]]
      coverages$schema_era[matched] <- scalar_schema[scalar_match[matched]]
      coverages$period_policy[matched] <-
        scalar_catalogue$period_policy[scalar_match[matched]]
      if (identical(dataset_id, "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION")) {
        snapshot_from <- as.Date(dataset$default_start[[1L]])
        snapshot_to <- as.Date(if (nzchar(dataset$available_to[[1L]])) {
          dataset$available_to[[1L]]
        } else {
          NA_character_
        })
        coverages$available_from[matched] <- pmin(
          coverages$available_from[matched], snapshot_from
        )
        if (is.na(snapshot_to)) {
          coverages$available_to[matched] <- as.Date(NA)
        } else {
          finite <- !is.na(coverages$available_to[matched])
          coverages$available_to[which(matched)[finite]] <- pmax(
            coverages$available_to[which(matched)[finite]], snapshot_to
          )
        }
        coverages$schema_era[matched] <- paste0(
          coverages$schema_era[matched], "|manifest_default"
        )
      }
      missing <- which(!matched)
      if (length(missing)) {
        fallback <- lapply(missing, function(tuple_index) {
          filters <- as.list(tuples[tuple_index, fields, drop = FALSE])
          .nbs_dataset_tuple_coverage(dataset_id, filters)
        })
        coverages$available_from[missing] <- as.Date(vapply(
          fallback, function(value) as.numeric(value$available_from), numeric(1)
        ), origin = "1970-01-01")
        coverages$available_to[missing] <- as.Date(vapply(
          fallback, function(value) as.numeric(value$available_to), numeric(1)
        ), origin = "1970-01-01")
        coverages$schema_era[missing] <- vapply(
          fallback, `[[`, character(1), "schema_era"
        )
      }
      uniform <- .nbs_uniform_dataset_coverage_rows(
        dataset_id, tuples, datasets$frequency[[index]], coverages,
        datasets$period_policy[[index]], portal_ids = portal_ids
      )
      if (!is.null(uniform)) return(list(uniform))
      return(lapply(seq_len(nrow(tuples)), function(tuple_index) {
        filters <- as.list(tuples[tuple_index, fields, drop = FALSE])
        .nbs_coverage_row(
          "dataset", dataset_id, dataset_id = dataset_id,
          dimension_key = tuples$dimension_key[[tuple_index]], filters = filters,
          frequency = datasets$frequency[[index]],
          available_from = coverages$available_from[[tuple_index]],
          available_to = coverages$available_to[[tuple_index]],
          schema_era = coverages$schema_era[[tuple_index]],
          period_policy = coverages$period_policy[[tuple_index]],
          portal_series_id = portal_ids[[tuple_index]]
        )
      }))
    }
    snapshot_only <- !portal_backed &&
      !"series_id" %in% fields
    if (snapshot_only) {
      coverages <- .nbs_dataset_snapshot_coverages(dataset_id, tuples)
      tenday <- .nbs_tenday_dataset_coverage_rows(
        dataset_id, tuples, datasets$frequency[[index]], coverages
      )
      if (!is.null(tenday)) return(list(tenday))
      uniform <- .nbs_uniform_dataset_coverage_rows(
        dataset_id, tuples, datasets$frequency[[index]], coverages,
        datasets$period_policy[[index]]
      )
      if (!is.null(uniform)) return(list(uniform))
      return(lapply(seq_len(nrow(tuples)), function(tuple_index) {
        filters <- as.list(tuples[tuple_index, fields, drop = FALSE])
        .nbs_coverage_row(
          "dataset", dataset_id, dataset_id = dataset_id,
          dimension_key = tuples$dimension_key[[tuple_index]], filters = filters,
          frequency = datasets$frequency[[index]],
          available_from = coverages$available_from[[tuple_index]],
          available_to = coverages$available_to[[tuple_index]],
          schema_era = coverages$schema_era[[tuple_index]],
          period_policy = datasets$period_policy[[index]]
        )
      }))
    }
    lapply(seq_len(nrow(tuples)), function(tuple_index) {
      filters <- as.list(tuples[tuple_index, fields, drop = FALSE])
      coverage <- .nbs_dataset_tuple_coverage(dataset_id, filters)
      .nbs_coverage_row(
        "dataset", dataset_id, dataset_id = dataset_id,
        dimension_key = tuples$dimension_key[[tuple_index]], filters = filters,
        frequency = datasets$frequency[[index]],
        available_from = coverage$available_from, available_to = coverage$available_to,
        schema_era = coverage$schema_era,
        period_policy = datasets$period_policy[[index]]
      )
    })
  }), recursive = FALSE)
  result <- do.call(rbind, c(scalar_rows, dataset_rows))
  rownames(result) <- NULL
  assign("audit", result, envir = .nbs_historical_coverage_cache)
  result
}

#' Assert that bundled NBS historical coverage satisfies policy
#'
#' @return Invisibly returns the audit data frame when every entity is covered.
#' @export
nbs_assert_historical_coverage <- function() {
  audit <- nbs_historical_coverage()
  failures <- audit[audit$status != "covered", , drop = FALSE]
  if (nrow(failures)) {
    preview <- utils::head(paste0(
      failures$entity_type, ":", failures$identity,
      ifelse(nzchar(failures$dimension_key), paste0("?", failures$dimension_key), ""),
      " starts ", failures$available_from,
      " but requires ", failures$required_from
    ), 8L)
    .nbs_abort(
      "nbs_historical_coverage_error",
      sprintf(
        "%d NBS entities have unexplained post-2010 starts: %s",
        nrow(failures), paste(preview, collapse = "; ")
      ),
      failures = failures
    )
  }
  invisible(audit)
}
