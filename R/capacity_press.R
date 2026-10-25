.nbs_capacity_industry_aliases <- function() {
  list(
    total = c(
      "industry", "total industry", "national industry",
      "industry above designated size", "\u5DE5\u4E1A", "\u5168\u56FD\u5DE5\u4E1A",
      "\u5168\u56FD\u89C4\u6A21\u4EE5\u4E0A\u5DE5\u4E1A", "\u89C4\u6A21\u4EE5\u4E0A\u5DE5\u4E1A"
    ),
    mining = c("mining", "mining and quarrying", "\u91C7\u77FF\u4E1A"),
    manufacturing = c("manufacturing", "\u5236\u9020\u4E1A"),
    utilities = c(
      "production and supply of electricity heat gas and water",
      "production and supply of electricity heat power gas and water",
      "production and supply of electricity thermal power gas and water",
      "production and supply of electric power heat power gas and water",
      "production and distribution of electricity heat gas and water",
      "production and distribution of electric power heat power gas and water",
      "production and distribution of electricity heating power gas and water",
      "electricity heat gas and water production and supply",
      "\u7535\u529B\u70ED\u529B\u71C3\u6C14\u53CA\u6C34\u751F\u4EA7\u548C\u4F9B\u5E94\u4E1A"
    ),
    coal_mining_washing = c(
      "mining and washing of coal", "coal mining and washing",
      "\u7164\u70AD\u5F00\u91C7\u548C\u6D17\u9009\u4E1A"
    ),
    oil_gas_extraction = c(
      "extraction of petroleum and natural gas",
      "oil and natural gas extraction", "\u77F3\u6CB9\u548C\u5929\u7136\u6C14\u5F00\u91C7\u4E1A"
    ),
    food_manufacturing = c(
      "manufacture of foods", "food manufacturing", "\u98DF\u54C1\u5236\u9020\u4E1A"
    ),
    textiles = c(
      "manufacture of textiles", "manufacture of textile",
      "textile industry", "textiles", "\u7EBA\u7EC7\u4E1A"
    ),
    chemicals = c(
      "manufacture of raw chemical materials and chemical products",
      "manufacture of chemical raw material and chemical products",
      "raw chemical materials and chemical products manufacturing",
      "\u5316\u5B66\u539F\u6599\u548C\u5316\u5B66\u5236\u54C1\u5236\u9020\u4E1A"
    ),
    medicines = c(
      "manufacture of medicines", "pharmaceutical manufacturing",
      "\u533B\u836F\u5236\u9020\u4E1A"
    ),
    chemical_fibres = c(
      "manufacture of chemical fibers", "manufacture of chemical fibres",
      "chemical fiber manufacturing", "chemical fibre manufacturing",
      "\u5316\u5B66\u7EA4\u7EF4\u5236\u9020\u4E1A"
    ),
    non_metallic_minerals = c(
      "manufacture of non metallic mineral products",
      "manufacture of nonmetal mineral products",
      "non metallic mineral products manufacturing", "\u975E\u91D1\u5C5E\u77FF\u7269\u5236\u54C1\u4E1A"
    ),
    ferrous_metals = c(
      "smelting and pressing of ferrous metals",
      "manufacture of ferrous metal smelting and pressing",
      "ferrous metal smelting and rolling processing",
      "\u9ED1\u8272\u91D1\u5C5E\u51B6\u70BC\u548C\u538B\u5EF6\u52A0\u5DE5\u4E1A"
    ),
    non_ferrous_metals = c(
      "smelting and pressing of non ferrous metals",
      "manufacture and processing of non ferrous metals",
      "non ferrous metal smelting and rolling processing",
      "\u6709\u8272\u91D1\u5C5E\u51B6\u70BC\u548C\u538B\u5EF6\u52A0\u5DE5\u4E1A"
    ),
    general_purpose_machinery = c(
      "manufacture of general purpose machinery",
      "general purpose machinery manufacturing", "\u901A\u7528\u8BBE\u5907\u5236\u9020\u4E1A"
    ),
    special_purpose_machinery = c(
      "manufacture of special purpose machinery",
      "special purpose machinery manufacturing", "\u4E13\u7528\u8BBE\u5907\u5236\u9020\u4E1A"
    ),
    automobiles = c(
      "manufacture of automobiles", "automobile manufacturing",
      "\u6C7D\u8F66\u5236\u9020\u4E1A"
    ),
    electrical_machinery = c(
      "manufacture of electrical machinery and apparatus",
      "manufacture of electric machinery and equipment",
      "electrical machinery and equipment manufacturing",
      "\u7535\u6C14\u673A\u68B0\u548C\u5668\u6750\u5236\u9020\u4E1A"
    ),
    computers_communication_electronics = c(
      paste(
        "manufacture of computers communication and other electronic",
        "equipment"
      ),
      paste(
        "manufacture of computers communication equipment and other",
        "electronic equipment"
      ),
      paste(
        "manufacture of computer communication equipment and other",
        "electronic equipment"
      ),
      paste(
        "computer communication and other electronic equipment",
        "manufacturing"
      ),
      "\u8BA1\u7B97\u673A\u901A\u4FE1\u548C\u5176\u4ED6\u7535\u5B50\u8BBE\u5907\u5236\u9020\u4E1A"
    )
  )
}

.nbs_capacity_normalize_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("^[[:space:]\u3000]*(\u5176\u4E2D|of which|including)[\uFF1A:]?", "", x,
    perl = TRUE
  )
  x <- gsub("[\uFF08\uFF09()\u3001\uFF0C,\u3002.;\uFF1B:\uFF1A/\\\\&%\uFF05\u00a0\u3000]", " ", x,
    perl = TRUE
  )
  x <- gsub("[-\u2010\u2011\u2012\u2013\u2014\u2212]", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

.nbs_capacity_industry_code <- function(labels) {
  aliases <- .nbs_capacity_industry_aliases()
  lookup <- unlist(lapply(names(aliases), function(code) {
    stats::setNames(
      rep(code, length(aliases[[code]])),
      .nbs_capacity_normalize_label(aliases[[code]])
    )
  }), use.names = TRUE)
  normalized <- .nbs_capacity_normalize_label(labels)
  codes <- unname(lookup[normalized])
  missing <- is.na(codes)
  if (any(missing)) {
    compact_lookup <- stats::setNames(
      unname(lookup), gsub(" ", "", names(lookup), fixed = TRUE)
    )
    codes[missing] <- unname(compact_lookup[
      gsub(" ", "", normalized[missing], fixed = TRUE)
    ])
  }
  codes
}

.nbs_capacity_quarter_number <- function(value) {
  value <- tolower(as.character(value))
  chinese <- c("\u4E00" = 1L, "\u4E8C" = 2L, "\u4E09" = 3L, "\u56DB" = 4L)
  english <- c(first = 1L, second = 2L, third = 3L, fourth = 4L)
  if (value %in% names(chinese)) {
    return(unname(chinese[[value]]))
  }
  if (value %in% names(english)) {
    return(unname(english[[value]]))
  }
  suppressWarnings(as.integer(value))
}

.nbs_capacity_release_quarter <- function(document, release = NULL) {
  if (!is.null(release) && "period" %in% names(release)) {
    period <- as.character(release$period[[1L]])
    hit <- regmatches(
      period,
      regexec("^((?:19|20)[0-9]{2})-Q([1-4])$", period)
    )[[1L]]
    if (length(hit) == 3L) {
      return(c(year = as.integer(hit[[2L]]), quarter = as.integer(hit[[3L]])))
    }
  }
  if (!is.null(release) && "reference_period_end" %in% names(release)) {
    end <- suppressWarnings(as.Date(release$reference_period_end[[1L]]))
    if (!is.na(end)) {
      return(c(
        year = as.integer(format(end, "%Y")),
        quarter = (as.integer(format(end, "%m")) - 1L) %/% 3L + 1L
      ))
    }
  }
  text <- if (is.null(document)) {
    ""
  } else {
    rvest::html_text2(rvest::html_element(document, "body"))
  }
  text <- gsub("[[:space:]\u00a0\u3000]+", " ", text, perl = TRUE)
  chinese <- regmatches(
    text,
    regexec("((?:19|20)[0-9]{2})\u5E74\u7B2C?([\u4E00\u4E8C\u4E09\u56DB1-4])\u5B63\u5EA6", text, perl = TRUE)
  )[[1L]]
  if (length(chinese) == 3L) {
    return(c(
      year = as.integer(chinese[[2L]]),
      quarter = .nbs_capacity_quarter_number(chinese[[3L]])
    ))
  }
  english_year_first <- regmatches(
    text,
    regexec(
      paste0(
        "((?:19|20)[0-9]{2})[ -]*(?:q([1-4])|",
        "(first|second|third|fourth) quarter)"
      ),
      text, ignore.case = TRUE, perl = TRUE
    )
  )[[1L]]
  if (length(english_year_first) == 4L) {
    token <- if (nzchar(english_year_first[[3L]])) {
      english_year_first[[3L]]
    } else {
      english_year_first[[4L]]
    }
    return(c(
      year = as.integer(english_year_first[[2L]]),
      quarter = .nbs_capacity_quarter_number(token)
    ))
  }
  english_quarter_first <- regmatches(
    text,
    regexec(
      "(first|second|third|fourth) quarter(?: of)? ((?:19|20)[0-9]{2})",
      text, ignore.case = TRUE, perl = TRUE
    )
  )[[1L]]
  if (length(english_quarter_first) == 3L) {
    return(c(
      year = as.integer(english_quarter_first[[3L]]),
      quarter = .nbs_capacity_quarter_number(english_quarter_first[[2L]])
    ))
  }
  .nbs_abort(
    "nbs_press_schema_error",
    "Could not determine the reference quarter of the capacity release."
  )
}

.nbs_capacity_candidate_table <- function(tables) {
  expected <- names(.nbs_capacity_industry_aliases())
  candidates <- lapply(seq_along(tables), function(table_index) {
    table <- as.data.frame(tables[[table_index]], stringsAsFactors = FALSE)
    if (ncol(table) < 3L || !nrow(table)) {
      return(NULL)
    }
    codes <- .nbs_capacity_industry_code(table[[1L]])
    numeric_cell_count <- vapply(seq_len(nrow(table)), function(row) {
      values <- trimws(as.character(table[row, -1L, drop = TRUE]))
      normalized <- gsub(",", "", values, fixed = TRUE)
      sum(grepl(
        "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$",
        normalized
      ))
    }, integer(1))
    rows <- which(!is.na(codes) & numeric_cell_count %in% c(2L, 4L, 5L))
    if (
      length(rows) != length(expected) || anyDuplicated(codes[rows]) ||
        !setequal(codes[rows], expected)
    ) {
      return(NULL)
    }
    numeric_columns <- which(vapply(seq_len(ncol(table)), function(column) {
      values <- trimws(as.character(table[[column]][rows]))
      normalized <- gsub(",", "", values, fixed = TRUE)
      all(grepl(
        "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$",
        normalized
      ))
    }, logical(1)))
    numeric_columns <- setdiff(numeric_columns, 1L)
    if (length(numeric_columns) == 5L) {
      adjacent <- cbind(
        numeric_columns[-length(numeric_columns)],
        numeric_columns[-1L]
      )
      duplicated_pairs <- which(vapply(seq_len(nrow(adjacent)), function(i) {
        identical(
          as.character(table[[adjacent[i, 1L]]][rows]),
          as.character(table[[adjacent[i, 2L]]][rows])
        )
      }, logical(1)))
      if (length(duplicated_pairs) != 1L) {
        return(NULL)
      }
      # A 2023 NBS page duplicates the first half-year rate column in its
      # responsive HTML. Drop the earlier member of the one exact adjacent
      # duplicate; the later column carries the correct half-year header.
      numeric_columns <- numeric_columns[-duplicated_pairs[[1L]]]
    }
    if (!length(numeric_columns) %in% c(2L, 4L)) {
      return(NULL)
    }
    list(
      table_index = table_index,
      table = table,
      rows = rows,
      codes = codes[rows],
      numeric_columns = numeric_columns
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) {
    .nbs_abort(
      "nbs_press_schema_error",
      paste(
        "No capacity-utilisation table contained the exact 19 published",
        "categories and either two or four numeric measure columns."
      )
    )
  }
  fingerprints <- vapply(candidates, function(candidate) {
    paste(
      candidate$codes,
      unlist(candidate$table[
        candidate$rows, candidate$numeric_columns, drop = FALSE
      ]),
      collapse = "\r"
    )
  }, character(1))
  if (length(unique(fingerprints)) != 1L) {
    .nbs_abort(
      "nbs_press_schema_error",
      "The release contained conflicting capacity-utilisation tables."
    )
  }
  candidates[[1L]]
}

.nbs_parse_capacity_utilisation_panel <- function(
    tables,
    document,
    dataset_id = "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION",
    release = NULL) {
  candidate <- .nbs_capacity_candidate_table(tables)
  reference <- .nbs_capacity_release_quarter(document, release)
  year <- unname(reference[["year"]])
  quarter <- unname(reference[["quarter"]])
  quarter_start <- as.Date(sprintf(
    "%04d-%02d-01", year, (quarter - 1L) * 3L + 1L
  ))
  quarter_end <- .nbs_next_period(quarter_start, "Q") - 1L
  ytd_start <- as.Date(sprintf("%04d-01-01", year))
  prior_quarter_start <- as.Date(sprintf(
    "%04d-%02d-01", year - 1L, (quarter - 1L) * 3L + 1L
  ))
  prior_quarter_end <- .nbs_next_period(prior_quarter_start, "Q") - 1L
  prior_ytd_start <- as.Date(sprintf("%04d-01-01", year - 1L))

  columns <- candidate$numeric_columns
  if (length(columns) == 2L) {
    measures <- c("current_percent", "current_yoy_change_pp")
    period_basis <- rep("current", 2L)
  } else {
    measures <- c(
      "current_percent", "current_yoy_change_pp",
      "ytd_percent", "ytd_yoy_change_pp"
    )
    period_basis <- c("current", "current", "ytd", "ytd")
  }
  records <- list()
  cursor <- 0L
  for (row_position in seq_along(candidate$rows)) {
    row_index <- candidate$rows[[row_position]]
    for (measure_index in seq_along(columns)) {
      column_index <- columns[[measure_index]]
      number <- .nbs_strict_number(
        candidate$table[[column_index]][[row_index]],
        "capacity-utilisation table"
      )
      cursor <- cursor + 1L
      record <- .nbs_panel_record(
        dataset_id,
        number$raw,
        number$value,
        candidate$table_index,
        row_index,
        column_index,
        row_label = trimws(candidate$table[[1L]][[row_index]]),
        industry = candidate$codes[[row_position]],
        measure = measures[[measure_index]],
        period_basis = period_basis[[measure_index]],
        unit = if (grepl("_pp$", measures[[measure_index]])) {
          "Percentage points"
        } else {
          "Percent"
        }
      )
      record$series_id <- paste(
        "NBS/CN/Q/CAPACITY_UTILISATION",
        toupper(candidate$codes[[row_position]]),
        toupper(measures[[measure_index]]),
        sep = "/"
      )
      is_ytd <- identical(period_basis[[measure_index]], "ytd")
      record$reported_observation_date <- quarter_start
      record$observation_period_start <- if (is_ytd) {
        ytd_start
      } else {
        quarter_start
      }
      record$observation_period_end <- quarter_end
      is_comparison <- grepl(
        "yoy_change_pp$", measures[[measure_index]]
      )
      record$comparison_period_start <- if (!is_comparison) {
        as.Date(NA)
      } else if (is_ytd) {
        prior_ytd_start
      } else {
        prior_quarter_start
      }
      record$comparison_period_end <- if (is_comparison) {
        prior_quarter_end
      } else {
        as.Date(NA)
      }
      record$comparison_basis <- if (is_comparison) {
        "same_period_previous_year"
      } else {
        "not_applicable"
      }
      record$published_value_type <- if (is_comparison) {
        "published_percentage_point_change"
      } else {
        "published_utilisation_rate"
      }
      record$schema_version <- "capacity-gbt4754-2017-detailed-v1"
      record$basket_version <- "GB/T4754-2017"
      record$classification_id <- "GB/T4754-2017"
      record$coverage_version <- if (year <= 2021L) {
        "approximately-90000-enterprises"
      } else {
        "approximately-110000-enterprises"
      }
      record$seasonal_adjustment <- "not_seasonally_adjusted"
      records[[cursor]] <- record
    }
  }
  result <- do.call(rbind, records)
  expected_cells <- length(.nbs_capacity_industry_aliases()) * length(columns)
  if (nrow(result) != expected_cells || anyDuplicated(paste(
    result$industry, result$measure, sep = "\r"
  ))) {
    .nbs_abort(
      "nbs_dataset_cardinality_error",
      sprintf(
        "The capacity release contained %d normalized cells; expected %d.",
        nrow(result), expected_cells
      ),
      expected_cells = expected_cells,
      actual_cells = nrow(result)
    )
  }
  rownames(result) <- NULL
  result
}
