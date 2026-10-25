.nbs_require_fields <- function(x, fields, context) {
  missing <- fields[!fields %in% names(x)]
  if (length(missing)) {
    .nbs_abort(
      "nbs_schema_error",
      sprintf(
        "%s is missing required fields: %s.",
        context,
        paste(missing, collapse = ", ")
      ),
      missing_fields = missing
    )
  }
  invisible(x)
}

.nbs_parse_value <- function(raw_value) {
  if (is.null(raw_value) || length(raw_value) == 0L) {
    return(list(raw = NA_character_, value = NA_real_))
  }
  if (length(raw_value) != 1L) {
    .nbs_abort(
      "nbs_value_error",
      "An NBS observation value was not scalar."
    )
  }
  if (is.numeric(raw_value)) {
    return(list(
      raw = as.character(raw_value),
      value = as.numeric(raw_value)
    ))
  }
  if (!is.character(raw_value) || is.na(raw_value)) {
    return(list(raw = NA_character_, value = NA_real_))
  }
  raw <- trimws(raw_value)
  missing_tokens <- c(
    "", "-", "--", "\u2013", "\u2014", "..", "...",
    "NA", "N/A", "NULL", "null"
  )
  if (raw %in% missing_tokens) {
    return(list(raw = raw, value = NA_real_))
  }
  normalized <- gsub(",", "", raw, fixed = TRUE)
  if (!grepl(
    "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$",
    normalized
  )) {
    .nbs_abort(
      "nbs_value_error",
      sprintf("Unrecognised non-numeric NBS value `%s`.", raw),
      raw_value = raw
    )
  }
  list(raw = raw, value = as.numeric(normalized))
}

.nbs_parse_observations <- function(response, slice, language) {
  payload <- response$payload
  if (is.null(payload$data) || !is.list(payload$data)) {
    .nbs_abort(
      "nbs_schema_error",
      "The NBS observation response has no list-valued `data` field.",
      raw_sha256 = response$raw_sha256
    )
  }
  if (!length(payload$data)) {
    .nbs_abort(
      "nbs_empty_data_error",
      sprintf(
        "NBS returned no period blocks for series `%s` and catalogue `%s`.",
        slice$series_id, slice$catalog_uuid
      ),
      series_id = slice$series_id,
      catalog_uuid = slice$catalog_uuid,
      raw_sha256 = response$raw_sha256
    )
  }

  rows <- vector("list", length(payload$data))
  for (block_index in seq_along(payload$data)) {
    block <- payload$data[[block_index]]
    if (!is.list(block)) {
      .nbs_abort(
        "nbs_schema_error",
        "An NBS period block is not an object.",
        raw_sha256 = response$raw_sha256
      )
    }
    .nbs_require_fields(
      block,
      c("code", "values"),
      sprintf("NBS period block %d", block_index)
    )
    if (!is.list(block$values)) {
      .nbs_abort(
        "nbs_schema_error",
        sprintf("NBS period block %d has a non-list `values` field.", block_index),
        raw_sha256 = response$raw_sha256
      )
    }
    if (length(block$values) != 1L) {
      .nbs_abort(
        "nbs_schema_error",
        sprintf(
          "NBS period `%s` returned %d value objects; exactly one was requested.",
          .nbs_as_character_scalar(block$code),
          length(block$values)
        ),
        raw_sha256 = response$raw_sha256
      )
    }
    matching <- Filter(
      function(value) {
        is.list(value) &&
          identical(.nbs_as_character_scalar(value[["_id"]]),
                    slice$indicator_uuid)
      },
      block$values
    )
    if (length(matching) != 1L) {
      .nbs_abort(
        "nbs_indicator_identity_error",
        sprintf(
          "NBS period `%s` contained %d matches for indicator `%s`.",
          .nbs_as_character_scalar(block$code),
          length(matching),
          slice$indicator_uuid
        ),
        indicator_uuid = slice$indicator_uuid,
        raw_sha256 = response$raw_sha256
      )
    }
    value <- matching[[1L]]
    .nbs_require_fields(
      value,
      c("_id", "value", "catalogid", "da"),
      sprintf("NBS observation in period `%s`", block$code)
    )
    if (!identical(
      .nbs_as_character_scalar(value$catalogid),
      slice$catalog_uuid
    )) {
      .nbs_abort(
        "nbs_catalogue_identity_error",
        sprintf(
          "NBS period `%s` returned catalogue `%s`, expected `%s`.",
          block$code,
          .nbs_as_character_scalar(value$catalogid),
          slice$catalog_uuid
        ),
        raw_sha256 = response$raw_sha256
      )
    }
    if (!identical(
      .nbs_as_character_scalar(value$da),
      slice$geography_code
    )) {
      .nbs_abort(
        "nbs_geography_identity_error",
        sprintf(
          "NBS period `%s` returned geography `%s`, expected `%s`.",
          block$code,
          .nbs_as_character_scalar(value$da),
          slice$geography_code
        ),
        raw_sha256 = response$raw_sha256
      )
    }

    period <- .nbs_decode_period(
      .nbs_as_character_scalar(block$code),
      slice$frequency
    )
    parsed_value <- .nbs_parse_value(value$value)
    span <- .nbs_observation_span(
      period$date,
      slice$frequency,
      slice$period_policy,
      slice$aggregation_basis
    )
    slice_end <- if (nzchar(slice$slice_end)) {
      as.Date(slice$slice_end)
    } else {
      as.Date(NA)
    }
    rows[[block_index]] <- data.frame(
      series_id = slice$series_id,
      dataset_id = slice$series_id,
      release_id = .nbs_series_release(.nbs_series_family(slice$series_id)),
      family = .nbs_series_family(slice$series_id),
      dimension_key = "",
      dimension_label = slice[[paste0("geography_", language)]],
      route = "portal_uuid_series",
      provider = "nbs",
      title = slice[[paste0("title_", language)]],
      title_en = slice$title_en,
      title_zh = slice$title_zh,
      series_note = slice[[paste0("series_note_", language)]],
      series_note_en = slice$series_note_en,
      series_note_zh = slice$series_note_zh,
      date = period$date,
      period = period$period,
      reference_date = period$date,
      reference_period = period$period,
      reference_period_start = span$start,
      reference_period_end = span$end,
      span_months = span$span_months,
      aggregation_basis = span$aggregation_basis,
      release_date = as.Date(NA),
      vintage_date = as.Date(NA),
      data_status = "latest_revised",
      seasonal_adjustment = slice$seasonal_adjustment,
      derived_method = "none",
      source_vintage = "latest_revised_database",
      source_observation_periods = period$period,
      source_raw_sha256 = response$raw_sha256,
      frequency = slice$frequency,
      value = parsed_value$value * slice$scale,
      raw_value = parsed_value$raw,
      is_missing = is.na(parsed_value$value),
      unit = slice[[paste0("unit_", language)]],
      unit_en = slice$unit_en,
      unit_zh = slice$unit_zh,
      raw_unit = .nbs_as_character_scalar(value$du_name),
      scale = slice$scale,
      geography_code = slice$geography_code,
      geography = slice[[paste0("geography_", language)]],
      raw_geography = .nbs_as_character_scalar(value$da_name),
      catalog_uuid = slice$catalog_uuid,
      indicator_uuid = slice$indicator_uuid,
      root_uuid = slice$root_uuid,
      classification_id = slice$classification_id,
      stitch_policy = slice$stitch_policy,
      period_policy = slice$period_policy,
      slice_start = as.Date(slice$slice_start),
      slice_end = slice_end,
      retrieved_at = response$retrieved_at,
      source_url = slice$source_url,
      source_note = slice$source_note,
      raw_sha256 = response$raw_sha256,
      manifest_version = slice$manifest_version,
      parser_version = "2",
      package_version = .nbs_package_version(),
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.nbs_parse_tree <- function(response) {
  data <- response$payload$data
  if (is.null(data) || !is.list(data)) {
    .nbs_abort(
      "nbs_schema_error",
      "The NBS tree response has no list-valued `data` field.",
      raw_sha256 = response$raw_sha256
    )
  }
  if (!length(data)) {
    result <- data.frame(
      catalog_uuid = character(),
      parent_uuid = character(),
      name = character(),
      type = character(),
      level = integer(),
      is_leaf = logical(),
      start_year = character(),
      end_year = character(),
      explanation = character(),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- lapply(seq_along(data), function(index) {
      node <- data[[index]]
      .nbs_require_fields(
        node,
        c("_id", "name", "type", "isLeaf"),
        sprintf("NBS tree node %d", index)
      )
      uuid <- .nbs_as_character_scalar(node[["_id"]])
      if (!grepl("^[0-9a-f]{32}$", uuid)) {
        .nbs_abort(
          "nbs_schema_error",
          sprintf("NBS tree node %d has an invalid UUID.", index)
        )
      }
      data.frame(
        catalog_uuid = uuid,
        parent_uuid = .nbs_as_character_scalar(node$treeinfo_pid),
        name = .nbs_as_character_scalar(node$name),
        type = .nbs_as_character_scalar(node$type),
        level = suppressWarnings(as.integer(
          .nbs_as_character_scalar(node$treeinfo_level)
        )),
        is_leaf = .nbs_as_logical_scalar(node$isLeaf),
        start_year = .nbs_as_character_scalar(node$sdate),
        end_year = .nbs_as_character_scalar(node$edate),
        explanation = .nbs_as_character_scalar(node$explain),
        stringsAsFactors = FALSE
      )
    })
    result <- do.call(rbind, rows)
  }
  attr(result, "raw_sha256") <- response$raw_sha256
  attr(result, "retrieved_at") <- response$retrieved_at
  attr(result, "source_url") <- response$url
  result
}

.nbs_parse_indicators <- function(response, catalog_id) {
  data <- response$payload$data
  if (is.null(data) || !is.list(data)) {
    .nbs_abort(
      "nbs_schema_error",
      "The NBS indicator response has no list-valued `data` field.",
      raw_sha256 = response$raw_sha256
    )
  }
  indicators <- if (!is.null(data$list)) data$list else data
  if (!is.list(indicators)) {
    .nbs_abort(
      "nbs_schema_error",
      "The NBS indicator response has no validated indicator list.",
      raw_sha256 = response$raw_sha256
    )
  }
  if (!length(indicators)) {
    result <- data.frame(
      indicator_uuid = character(),
      catalog_uuid = character(),
      name = character(),
      display_name = character(),
      economic_key = character(),
      period_dimension = character(),
      unit_uuid = character(),
      unit = character(),
      start_date = character(),
      end_date = character(),
      annotation = character(),
      note = character(),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- lapply(seq_along(indicators), function(index) {
      indicator <- indicators[[index]]
      .nbs_require_fields(
        indicator,
        c("_id", "catalogid", "i_showname", "dp"),
        sprintf("NBS indicator %d", index)
      )
      indicator_uuid <- .nbs_as_character_scalar(indicator[["_id"]])
      returned_catalog <- .nbs_as_character_scalar(indicator$catalogid)
      if (!grepl("^[0-9a-f]{32}$", indicator_uuid)) {
        .nbs_abort(
          "nbs_schema_error",
          sprintf("NBS indicator %d has an invalid UUID.", index)
        )
      }
      if (!identical(returned_catalog, catalog_id)) {
        .nbs_abort(
          "nbs_catalogue_identity_error",
          sprintf(
            "NBS indicator %d belongs to catalogue `%s`, expected `%s`.",
            index, returned_catalog, catalog_id
          )
        )
      }
      data.frame(
        indicator_uuid = indicator_uuid,
        catalog_uuid = returned_catalog,
        name = .nbs_first_character(
          indicator[["_name"]],
          indicator$kj2_name,
          indicator$kj1_name,
          indicator$i_showname
        ),
        display_name = .nbs_as_character_scalar(indicator$i_showname),
        economic_key = .nbs_first_character(
          indicator$ek,
          indicator$i,
          indicator$kj2_name,
          indicator$kj1_name
        ),
        period_dimension = .nbs_as_character_scalar(indicator$dp),
        unit_uuid = .nbs_as_character_scalar(indicator$du),
        unit = .nbs_as_character_scalar(indicator$du_name),
        start_date = .nbs_as_character_scalar(indicator$daterange_sdate),
        end_date = .nbs_as_character_scalar(indicator$daterange_edate),
        annotation = .nbs_as_character_scalar(indicator$i_annotation),
        note = .nbs_as_character_scalar(indicator$i_mark),
        stringsAsFactors = FALSE
      )
    })
    result <- do.call(rbind, rows)
  }
  attr(result, "raw_sha256") <- response$raw_sha256
  attr(result, "retrieved_at") <- response$retrieved_at
  attr(result, "source_url") <- response$url
  result
}
