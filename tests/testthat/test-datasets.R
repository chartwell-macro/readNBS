press_fixture_cases <- data.frame(
  dataset_id = c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY",
    "NBS/CN/M/HOUSING_PRICES/70_CITY",
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
    "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION",
    "NBS/CN/M/RETAIL_SALES/DETAIL",
    "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
    "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION",
    "NBS/CN/A/EMPLOYMENT_WAGES",
    "NBS/CN/A/MIGRANT_WORKERS",
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
  ),
  fixture = c(
    "press-industrial.html",
    "press-industrial.html",
    "press-housing.html",
    "press-property.html",
    "press-property.html",
    "press-retail.html",
    "press-demography.html",
    "press-households.html",
    "press-wages.html",
    "press-migrant.html",
    "press-tenday.html"
  ),
  expected = c(112L, 252L, 1680L, 7L, 32L, 115L, 5L, 6L, 315L, 139L, 153L),
  stringsAsFactors = FALSE
)

test_that("dataset catalogue is frozen and uses application enums", {
  testthat::local_mocked_bindings(
    nbs_catalogue = function(...) {
      stop("the scalar catalogue must not be loaded")
    },
    .package = "readNBS"
  )
  catalogue <- nbs_dataset_catalogue()
  expect_equal(nrow(catalogue), 39L)
  expect_equal(anyDuplicated(catalogue$dataset_id), 0L)
  expect_setequal(
    catalogue$period_policy,
    c(
      "all_periods",
      "january_not_reported_february_first_period",
      "ytd_january_not_reported_february_combined",
      "mixed_jan_feb_combined_then_monthly"
    )
  )
  expect_true(all(catalogue$aggregation_basis %in% c(
    "single_period", "year_to_date", "mixed_span", "irregular_window"
  )))
  expect_true(all(catalogue$seasonal_adjustment %in% c(
    "not_seasonally_adjusted", "not_applicable"
  )))
  expect_true(all(catalogue$derived_method == "none"))
  expect_equal(
    as.integer(table(catalogue$history_role)[c(
      "both", "continuous", "exact_source"
    )]),
    c(11L, 6L, 22L)
  )
  expect_true(all(nzchar(catalogue$continuous_dataset_id[
    catalogue$history_role != "exact_source"
  ])))
  exact_replacements <- catalogue$continuous_dataset_id[
    catalogue$history_role == "exact_source" &
      nzchar(catalogue$continuous_dataset_id)
  ]
  expect_true(all(exact_replacements %in% catalogue$dataset_id))
  expect_false(any(
    catalogue$history_role[match(
      exact_replacements, catalogue$dataset_id
    )] == "exact_source"
  ))
  expect_identical(
    catalogue$continuous_dataset_id[catalogue$dataset_id ==
      "NBS/CN/A/EMPLOYMENT_WAGES"],
    "NBS/CN/A/WAGES/LONGITUDINAL"
  )
  expect_identical(
    catalogue$continuous_dataset_id[catalogue$dataset_id ==
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES"],
    ""
  )
  expect_true(all(catalogue$source_vintage %in% c(
    "latest_revised_database",
    "first_release_snapshot",
    "first_release_press_archive",
    "first_release_and_latest_revised",
    "retained_yearbook_snapshot",
    "retained_official_yearbook_legacy_snapshot",
    "published_release_snapshot",
    "retained_official_chart_history",
    "retained_first_release_headlines",
    "latest_retained_official_observation"
  )))

  expected_portal_starts <- c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" = "1999-01-01",
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL" = "2000-01-01",
    "NBS/CN/M/RETAIL_SALES/DETAIL" = "1990-01-01",
    "NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING" = "1990-01-01",
    "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION" = "2010-01-01"
  )
  actual_portal_starts <- catalogue$available_from[
    match(names(expected_portal_starts), catalogue$dataset_id)
  ]
  expect_identical(
    actual_portal_starts,
    as.Date(unname(expected_portal_starts))
  )
})

test_that("all dataset routes declare the canonical observation provenance", {
  catalogue <- nbs_dataset_catalogue("en")
  required <- readNBS:::.nbs_dataset_observation_provenance_fields
  scalar_schema <- names(readNBS:::.nbs_empty_observations())

  expect_equal(nrow(catalogue), 39L)
  expect_setequal(unique(catalogue$route), c(
    "portal_uuid_panel", "portal_uuid_plus_press",
    "portal_uuid_tree_plus_press", "press_html_table",
    "press_release_table", "retained_longitudinal_series",
    "yearbook_snapshot"
  ))
  expect_true(all(required %in% scalar_schema))

  descriptor_fields <- setdiff(required, "package_version")
  for (dataset_id in catalogue$dataset_id) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    filters <- as.list(tuples[1L, setdiff(
      names(tuples), "dimension_key"
    ), drop = FALSE])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
    expect_true(
      all(descriptor_fields %in% names(descriptor)),
      info = dataset_id
    )
    expect_false(any(vapply(
      descriptor[descriptor_fields],
      function(value) anyNA(value) || any(!nzchar(trimws(as.character(value)))),
      logical(1L)
    )), info = dataset_id)
  }
})

test_that("every dataset route returns canonical non-generic units", {
  industry_id <- "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY"
  industry_tuples <- nbs_dataset_valid_tuples(industry_id)
  industry_filters <- as.list(industry_tuples[1L, setdiff(
    names(industry_tuples), "dimension_key"
  ), drop = FALSE])
  industry_descriptor <- nbs_resolve_dataset_series(
    industry_id, industry_filters
  )

  cases <- list(
    list(
      dataset_id = "NBS/CN/M/CPI/PRICE_DETAIL",
      filters = list(
        residence = "urban", comparison = "mom_index", category = "headline"
      ),
      result = nbs_fetch_dataset(
        "NBS/CN/M/CPI/PRICE_DETAIL",
        list(
          residence = "urban", comparison = "mom_index", category = "headline"
        ),
        "2010-01", "2010-01",
        client = nbs_client(
          transport = make_observation_transport(list(
            "6d767f323c1b427684637dc1a027cef1::201001MM-201001MM" =
              "cpi-urban-mom-2010.json"
          )),
          retries = 0L,
          now = fixed_now
        )
      )
    ),
    list(
      dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      filters = list(
        category = "total", measure = "business_revenue_level",
        period_basis = "ytd"
      ),
      result = nbs_fetch_dataset(
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
        list(
          category = "total", measure = "business_revenue_level",
          period_basis = "ytd"
        ),
        client = make_document_client("press-industrial.html")
      )
    ),
    list(
      dataset_id = "NBS/CN/M/RETAIL_SALES/DETAIL",
      filters = list(
        category_code = "total_retail_sales_of_consumer_goods",
        measure = "level", period_basis = "current"
      ),
      result = nbs_fetch_dataset(
        "NBS/CN/M/RETAIL_SALES/DETAIL",
        list(
          category_code = "total_retail_sales_of_consumer_goods",
          measure = "level", period_basis = "current"
        ),
        client = make_document_client("press-retail.html")
      )
    ),
    list(
      dataset_id = industry_id,
      filters = industry_filters,
      result = nbs_fetch_dataset(
        industry_id, industry_filters,
        industry_descriptor$available_from,
        industry_descriptor$available_to
      )
    ),
    list(
      dataset_id = "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
      filters = list(series = "survey_households"),
      result = nbs_fetch_dataset(
        "NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012",
        list(series = "survey_households")
      )
    ),
    list(
      dataset_id = "NBS/CN/A/YEARBOOK_DEMOGRAPHY_LEGACY",
      filters = list(series = "yb_1d79d069bad3ce75"),
      result = nbs_fetch_dataset(
        "NBS/CN/A/YEARBOOK_DEMOGRAPHY_LEGACY",
        list(series = "yb_1d79d069bad3ce75"),
        "2010", "2010",
        client = nbs_client(
          transport = function(request) stop("Yearbook fetch used network")
        )
      )
    ),
    list(
      dataset_id = "NBS/CN/A/WAGES/LONGITUDINAL",
      filters = list(
        unit_type = "non_private", breakdown = "overall",
        category = "total", measure = "average_wage"
      ),
      result = nbs_fetch_dataset(
        "NBS/CN/A/WAGES/LONGITUDINAL",
        list(
          unit_type = "non_private", breakdown = "overall",
          category = "total", measure = "average_wage"
        ),
        client = nbs_client(
          transport = function(request) stop("Longitudinal fetch used network")
        )
      )
    )
  )
  catalogue <- nbs_dataset_catalogue("en")
  expect_setequal(
    catalogue$route[match(
      vapply(cases, `[[`, "", "dataset_id"), catalogue$dataset_id
    )],
    unique(catalogue$route)
  )

  for (case in cases) {
    descriptor <- nbs_resolve_dataset_series(
      case$dataset_id, case$filters, "en"
    )
    result <- case$result
    expect_true(nrow(result) > 0L, info = case$dataset_id)
    expect_true(
      all(readNBS:::.nbs_dataset_observation_provenance_fields %in%
        names(result)),
      info = case$dataset_id
    )
    expect_true(all(result$unit == descriptor$unit), info = case$dataset_id)
    expect_false(any(grepl(
      "official (published|product) unit|varies|various",
      result$unit,
      ignore.case = TRUE
    )), info = case$dataset_id)
  }

  industry <- cases[[4L]]$result
  expect_lte(min(industry$date), as.Date("2024-06-01"))
  expect_gte(max(industry$date), as.Date("2026-06-01"))
  expect_true(all(industry$unit == "100 million yuan"))
})

test_that("indexed release dates and reference windows are coherent", {
  release_index <- readNBS:::.nbs_load_release_index()
  expect_gt(nrow(release_index), 11L)
  expect_equal(anyDuplicated(paste(
    release_index$dataset_id, release_index$period
  )), 0L)
  expect_true(all(
    as.Date(release_index$reference_period_start) <=
      as.Date(release_index$reference_period_end)
  ))
  expect_true(all(
    as.Date(release_index$release_date) >=
      as.Date(release_index$reference_period_end)
  ))
})

test_that("valid tuple tables enumerate only published exact series", {
  expected <- c(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" = 112L,
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_INDUSTRY" = 252L,
    "NBS/CN/M/HOUSING_PRICES/70_CITY" = 8960L,
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION" = 76L,
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL" = 70L,
    "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION" = 56L,
    "NBS/CN/M/RETAIL_SALES/DETAIL" = 103L,
    "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE" = 7L,
    "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION" = 98L,
    "NBS/CN/A/EMPLOYMENT_WAGES" = 315L,
    "NBS/CN/A/MIGRANT_WORKERS" = 139L,
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES" = 330L,
    "NBS/CN/M/CPI/PRICE_DETAIL" = 14L,
    "NBS/CN/M/PPI/PRICE_DETAIL" = 122L
  )
  catalogue <- nbs_dataset_catalogue()
  for (dataset_id in names(expected)) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    required <- strsplit(
      catalogue$required_dimensions[catalogue$dataset_id == dataset_id],
      "|",
      fixed = TRUE
    )[[1L]]
    expect_named(
      tuples,
      c(required, "dimension_key"),
      ignore.order = FALSE,
      info = dataset_id
    )
    expect_equal(nrow(tuples), expected[[dataset_id]], info = dataset_id)
    expect_equal(anyDuplicated(tuples$dimension_key), 0L, info = dataset_id)
  }
})

test_that("all published exact descriptors are complete and canonical", {
  catalogue <- nbs_dataset_catalogue("en")
  cjk <- "[\u3400-\u4DBF\u4E00-\u9FFF]"
  generic_unit <- paste(
    "official published unit", "official product unit", "varies", "various",
    sep = "|"
  )
  resolved <- 0L
  expected_resolved <- 0L

  for (dataset_id in catalogue$dataset_id) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    filter_names <- setdiff(names(tuples), "dimension_key")
    dimensions <- nbs_dataset_dimensions(dataset_id, "en")
    dataset_title <- catalogue$title_en[catalogue$dataset_id == dataset_id]
    # Exact tuple enumeration above remains exhaustive. Descriptor resolution
    # is intentionally bounded for very large official panels so CI does not
    # repeat the same cached validation tens of thousands of times.
    indices <- if (nrow(tuples) <= 500L) {
      seq_len(nrow(tuples))
    } else {
      unique(as.integer(round(seq(1, nrow(tuples), length.out = 100L))))
    }
    expected_resolved <- expected_resolved + length(indices)
    for (index in indices) {
      filters <- as.list(tuples[index, filter_names, drop = FALSE])
      descriptor <- nbs_resolve_dataset_series(
        dataset_id, filters[rev(names(filters))], "en"
      )
      info <- paste(dataset_id, tuples$dimension_key[[index]])

      expect_equal(nrow(descriptor), 1L, info = info)
      expect_identical(
        descriptor$dimension_key[[1L]], tuples$dimension_key[[index]],
        info = info
      )
      expected_labels <- vapply(filter_names, function(name) {
        dimensions$label_en[
          dimensions$dimension == name &
            dimensions$value == filters[[name]]
        ][[1L]]
      }, character(1))
      expect_identical(
        descriptor$title_en[[1L]],
        paste(
          dataset_title, paste(expected_labels, collapse = " \u2014 "),
          sep = " \u2014 "
        ),
        info = info
      )
      expect_true(all(nzchar(c(
        descriptor$title[[1L]], descriptor$title_en[[1L]],
        descriptor$unit[[1L]], descriptor$unit_en[[1L]],
        descriptor$series_note[[1L]], descriptor$series_note_en[[1L]]
      ))), info = info)
      native_migrant <- identical(
        dataset_id, "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"
      )
      if (!native_migrant) {
        expect_false(any(grepl(
          cjk,
          c(
            descriptor$title[[1L]], descriptor$title_en[[1L]],
            descriptor$unit[[1L]], descriptor$unit_en[[1L]],
            descriptor$series_note[[1L]], descriptor$series_note_en[[1L]]
          ),
          perl = TRUE
        )), info = info)
      }
      expect_false(grepl(
        generic_unit, descriptor$unit_en[[1L]], ignore.case = TRUE
      ), info = info)
      resolved <- resolved + 1L
    }
  }
  expect_equal(resolved, expected_resolved)
  expect_gt(resolved, 2000L)
})

test_that("representative exact descriptors ignore filter insertion order", {
  cases <- list(
    list(
      dataset_id = "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      filters = list(
        category = "total", measure = "business_revenue_level",
        period_basis = "ytd"
      )
    ),
    list(
      dataset_id = "NBS/CN/M/HOUSING_PRICES/70_CITY",
      filters = list(
        city_code = "110000", market = "new", size_band = "all",
        comparison = "mom", index_base = "2025"
      )
    ),
    list(
      dataset_id = "NBS/CN/M/RETAIL_SALES/DETAIL",
      filters = list(
        category_code = "total_retail_sales_of_consumer_goods",
        measure = "level", period_basis = "current"
      )
    ),
    list(
      dataset_id = "NBS/CN/A/EMPLOYMENT_WAGES",
      filters = list(
        unit_type = "private", region = "not_applicable",
        industry = "manufacturing", registration = "not_applicable",
        occupation = "not_applicable", measure = "current_year_average_wage"
      )
    ),
    list(
      dataset_id = "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
      filters = list(product_code = "p001", measure = "price_yuan")
    )
  )
  for (case in cases) {
    canonical <- nbs_resolve_dataset_series(case$dataset_id, case$filters)
    reversed <- nbs_resolve_dataset_series(
      case$dataset_id, case$filters[rev(names(case$filters))]
    )
    expect_identical(reversed, canonical, info = case$dataset_id)
  }
})

test_that("advertised portal and hybrid leaves are exact and resolvable", {
  expected <- c(
    "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION" = 76L,
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL" = 70L,
    "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION" = 98L
  )
  required_schema <- c(
    "dataset_id", "dimension_key", "title", "title_en", "title_zh",
    "unit", "unit_en", "unit_zh",
    "specification", "specification_en", "specification_zh",
    "product_unit", "product_unit_en", "product_unit_zh",
    "frequency", "period_policy",
    "aggregation_basis", "seasonal_adjustment", "derived_method",
    "source_vintage", "route", "release_id", "family", "available_from",
    "available_to", "series_note", "series_note_en", "series_note_zh",
    "source_url", "source_note", "manifest_version"
  )
  for (dataset_id in names(expected)) {
    dimensions <- nbs_dataset_dimensions(dataset_id)
    leaves <- dimensions$value[dimensions$dimension == "series_id"]
    expect_equal(length(leaves), expected[[dataset_id]], info = dataset_id)
    direct <- readNBS:::.nbs_dataset_series_candidates(dataset_id)
    expect_true(all(direct %in% leaves), info = dataset_id)
    descriptor <- nbs_resolve_dataset_series(
      dataset_id, list(series_id = leaves[[1L]])
    )
    expect_equal(nrow(descriptor), 1L, info = dataset_id)
    expect_named(descriptor, required_schema, ignore.order = FALSE)
    expect_false(any(grepl(
      "varies|various", descriptor$unit_en, ignore.case = TRUE
    )), info = dataset_id)
    for (series_id in setdiff(leaves, direct)) {
      synthetic <- nbs_resolve_dataset_series(
        dataset_id, list(series_id = series_id)
      )
      expect_equal(nrow(synthetic), 1L, info = series_id)
      expect_named(synthetic, required_schema, ignore.order = FALSE)
    }
  }
})

test_that("press fixtures map to their frozen release cardinalities", {
  manifest <- readNBS:::.nbs_load_dataset_manifest()
  release_index <- readNBS:::.nbs_load_release_index()
  for (index in seq_len(nrow(press_fixture_cases))) {
    case <- press_fixture_cases[index, , drop = FALSE]
    dataset <- manifest[
      manifest$dataset_id == case$dataset_id, , drop = FALSE
    ]
    parsed <- readNBS:::.nbs_parse_press_release(
      fixture_body(case$fixture), dataset
    )
    expect_equal(nrow(parsed), case$expected, info = case$dataset_id)
    tuples <- nbs_dataset_valid_tuples(case$dataset_id)
    required <- setdiff(names(tuples), "dimension_key")
    parsed_filters <- parsed[, sub(
      "^category_code$", "category", required
    ), drop = FALSE]
    names(parsed_filters) <- required
    parsed_keys <- unique(vapply(seq_len(nrow(parsed_filters)), function(row) {
      nbs_dimension_key(as.list(parsed_filters[row, , drop = FALSE]))
    }, character(1)))
    expect_true(
      all(parsed_keys %in% tuples$dimension_key),
      info = case$dataset_id
    )
    if (!case$dataset_id %in% c(
      "NBS/CN/M/HOUSING_PRICES/70_CITY",
      "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
      "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION",
      "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION",
      "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE",
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
    )) {
      expect_setequal(parsed_keys, tuples$dimension_key)
    }
  }
})

test_that("the documented empty-filter default returns complete panels", {
  ten_day <- nbs_fetch_dataset(
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
    client = make_document_client("press-tenday.html")
  )
  expect_equal(nrow(ten_day), 153L)
  expect_equal(length(unique(ten_day$dimension_key)), 153L)

  property <- nbs_fetch_dataset(
    "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL",
    client = make_document_client("press-property.html")
  )
  expect_equal(nrow(property), 7L)
  expect_equal(length(unique(property$dimension_key)), 7L)
})

test_that("annual wage tuples follow the eight published table shapes", {
  dataset_id <- "NBS/CN/A/EMPLOYMENT_WAGES"
  valid <- list(
    unit_type = "private",
    region = "not_applicable",
    industry = "manufacturing",
    registration = "not_applicable",
    occupation = "not_applicable",
    measure = "current_year_average_wage"
  )
  expect_equal(
    nrow(nbs_resolve_dataset_series(dataset_id, valid)),
    1L
  )

  private_registration <- valid
  private_registration$industry <- "not_applicable"
  private_registration$registration <- "domestic_invested_enterprises"
  expect_error(
    nbs_resolve_dataset_series(dataset_id, private_registration),
    class = "nbs_dataset_filter_error"
  )

  private_public_management <- valid
  private_public_management$industry <-
    "public_management_social_security_and_social_organizations"
  expect_error(
    nbs_resolve_dataset_series(dataset_id, private_public_management),
    class = "nbs_dataset_filter_error"
  )

  above_agriculture <- valid
  above_agriculture$unit_type <- "above_designated"
  above_agriculture$industry <-
    "agriculture_forestry_animal_husbandry_and_fishery"
  above_agriculture$occupation <-
    "professional_and_technical_personnel"
  above_agriculture$measure <- "occupation_average_wage"
  expect_error(
    nbs_resolve_dataset_series(dataset_id, above_agriculture),
    class = "nbs_dataset_filter_error"
  )

  manifest <- readNBS:::.nbs_load_dataset_manifest()
  dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  parsed <- readNBS:::.nbs_parse_press_release(
    fixture_body("press-wages.html"), dataset
  )
  expect_equal(nrow(parsed), 315L)
  keys <- vapply(seq_len(nrow(parsed)), function(index) {
    filters <- as.list(parsed[index, c(
      "unit_type", "region", "industry", "registration",
      "occupation", "measure"
    ), drop = FALSE])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
    expect_equal(nrow(descriptor), 1L)
    descriptor$dimension_key[[1L]]
  }, character(1))
  expect_equal(anyDuplicated(keys), 0L)
})

test_that("industrial end-period ratios preserve exact period and units", {
  dataset_id <- "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE"
  filters <- list(
    category = "total",
    measure = "revenue_per_person",
    period_basis = "end_period"
  )
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
  fetched <- nbs_fetch_dataset(
    dataset_id,
    filters,
    client = make_document_client("press-industrial.html")
  )
  expect_equal(nrow(fetched), 1L)
  expect_equal(fetched$period_policy, descriptor$period_policy)
  expect_equal(fetched$aggregation_basis, descriptor$aggregation_basis)
  expect_equal(fetched$unit, descriptor$unit)
  expect_equal(fetched$unit, "10,000 yuan per person")
  expect_equal(fetched$reference_period_start, as.Date("2026-05-01"))
  expect_equal(fetched$reference_period_end, as.Date("2026-05-31"))
})

test_that("large official panels preserve their real members", {
  manifest <- readNBS:::.nbs_load_dataset_manifest()
  parse <- function(dataset_id, fixture) {
    dataset <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
    readNBS:::.nbs_parse_press_release(fixture_body(fixture), dataset)
  }
  housing <- parse(
    "NBS/CN/M/HOUSING_PRICES/70_CITY", "press-housing.html"
  )
  expect_equal(length(unique(housing$city_code)), 70L)
  expect_true(all(grepl("^[0-9]{6}$", housing$city_code)))
  expect_equal(
    unique(housing$city_code[housing$city_label == "Beijing"]), "110000"
  )
  expect_equal(nrow(housing), 70L * 2L * 4L * 3L)

  prices <- parse(
    "NBS/CN/10D/PRODUCTION_MARKET_PRICES", "press-tenday.html"
  )
  expect_equal(length(unique(prices$product_code[
    prices$product_code != "summary"
  ])), 50L)
  expect_equal(nrow(prices), 50L * 3L + 3L)

  migrant <- parse("NBS/CN/A/MIGRANT_WORKERS", "press-migrant.html")
  expect_equal(nrow(migrant), 139L)
  expect_setequal(unique(migrant$section), paste0("table_", 1:5))
  expect_equal(length(unique(migrant$dimension_label_en)), 139L)
  expect_setequal(
    unique(migrant$unit_en),
    c("10,000 persons", "Percent", "Percentage points", "Yuan")
  )
  expect_false(any(grepl(
    "[\u3400-\u4DBF\u4E00-\u9FFF]",
    migrant$dimension_label_en,
    perl = TRUE
  )))
})

test_that("migrant report is one exact meaningful selector per cell", {
  dataset_id <- "NBS/CN/A/MIGRANT_WORKERS"
  dimensions <- nbs_dataset_dimensions(dataset_id)
  expect_setequal(unique(dimensions$dimension), "measure")
  expect_equal(nrow(dimensions), 139L)
  expect_equal(anyDuplicated(dimensions$value), 0L)
  expect_equal(anyDuplicated(dimensions$label_en), 0L)
  expect_equal(
    as.integer(table(dimensions$unit_en)[
      c("10,000 persons", "Percent", "Percentage points", "Yuan")
    ]),
    c(42L, 74L, 9L, 14L)
  )
  for (measure in dimensions$value) {
    descriptor <- nbs_resolve_dataset_series(
      dataset_id, list(measure = measure)
    )
    expect_equal(nrow(descriptor), 1L, info = measure)
    expect_false(grepl(
      "[\u3400-\u4DBF\u4E00-\u9FFF]",
      descriptor$title_en,
      perl = TRUE
    ), info = measure)
    expect_true(nzchar(descriptor$unit_en), info = measure)
  }

  first_measure <- dimensions$value[[1L]]
  result <- nbs_fetch_dataset(
    dataset_id,
    list(measure = first_measure),
    client = make_document_client("press-migrant.html")
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$unit, dimensions$unit_en[[1L]])
  expect_equal(result$dimension_label, dimensions$label_en[[1L]])
})

test_that("ten-day tuples carry product-specific units and specifications", {
  dataset_id <- "NBS/CN/10D/PRODUCTION_MARKET_PRICES"
  tonne <- nbs_resolve_dataset_series(
    dataset_id,
    list(product_code = "p001", measure = "price_yuan")
  )
  kilogram <- nbs_resolve_dataset_series(
    dataset_id,
    list(product_code = "p033", measure = "price_yuan")
  )
  summary <- nbs_resolve_dataset_series(
    dataset_id,
    list(product_code = "summary", measure = "increased_product_count")
  )
  expect_equal(tonne$unit_en, "Yuan per tonne")
  expect_equal(tonne$product_unit_en, "Tonne")
  expect_equal(tonne$specification_en, "\u03a620mm, HRB400E")
  expect_equal(kilogram$unit_en, "Yuan per kilogram")
  expect_equal(kilogram$product_unit_en, "Kilogram")
  expect_equal(kilogram$specification_en, "Dense Grade")
  expect_equal(summary$unit_en, "Products")
  expect_equal(summary$specification_en, "")
  expect_false(any(grepl(
    "official product unit|published unit", c(
      tonne$unit_en, kilogram$unit_en, summary$unit_en
    ),
    ignore.case = TRUE
  )))

  client <- make_document_client("press-tenday.html")
  fetched_tonne <- nbs_fetch_dataset(
    dataset_id,
    list(product_code = "p001", measure = "price_yuan"),
    client = client
  )
  fetched_kilogram <- nbs_fetch_dataset(
    dataset_id,
    list(product_code = "p033", measure = "price_yuan"),
    client = client
  )
  fetched_summary <- nbs_fetch_dataset(
    dataset_id,
    list(product_code = "summary", measure = "increased_product_count"),
    client = client
  )
  expect_equal(fetched_tonne$unit, "Yuan per tonne")
  expect_equal(fetched_tonne$specification, "\u03a620mm, HRB400E")
  expect_equal(fetched_kilogram$unit, "Yuan per kilogram")
  expect_equal(fetched_kilogram$specification, "Dense Grade")
  expect_equal(fetched_summary$unit, "Products")
  expect_equal(fetched_summary$value, 17)
  expect_true(is.na(fetched_tonne$span_months))
  expect_equal(fetched_tonne$date, as.Date("2026-07-01"))
})

test_that("exact press tuples return distinct one-row values and metadata", {
  client <- make_document_client("press-industrial.html")
  revenue <- nbs_fetch_dataset(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    filters = list(
      category = "total",
      measure = "business_revenue_level",
      period_basis = "ytd"
    ),
    start = "2026-05",
    end = "2026-05",
    client = client
  )
  profit <- nbs_fetch_dataset(
    "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
    filters = list(
      category = "total",
      measure = "total_profit_level",
      period_basis = "ytd"
    ),
    start = "2026-05",
    end = "2026-05",
    client = client
  )
  expect_equal(nrow(revenue), 1L)
  expect_equal(nrow(profit), 1L)
  expect_equal(revenue$value, 565506.8)
  expect_false(identical(revenue$value, profit$value))
  expect_equal(revenue$aggregation_basis, "year_to_date")
  expect_equal(
    revenue$period_policy,
    "ytd_january_not_reported_february_combined"
  )
  expect_equal(revenue$reference_period_start, as.Date("2026-01-01"))
  expect_equal(revenue$reference_period_end, as.Date("2026-05-31"))
  expect_equal(revenue$date, as.Date("2026-05-01"))
  expect_equal(revenue$span_months, 5L)
  expect_true(nzchar(revenue$series_note))
  expect_true(nzchar(revenue$series_note_en))
  expect_true(nzchar(revenue$series_note_zh))
  expect_true(
    revenue$source_url %in% readNBS:::.nbs_load_release_index()$source_url[
      readNBS:::.nbs_load_release_index()$dataset_id ==
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE"
    ]
  )
  expect_equal(
    revenue$dimension_key,
    paste0(
      "category=total|measure=business_revenue_level|",
      "period_basis=ytd"
    )
  )
})

test_that("hybrid portal leaves preserve resolver and fetch route identity", {
  dataset_id <- "NBS/CN/M/PROPERTY_DEVELOPMENT/NATIONAL"
  series_id <- "NBS/CN/M/PROPERTY/INVESTMENT/YTD_LEVEL"
  filters <- list(series_id = series_id)
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
  scalar <- nbs_manifest()
  scalar <- scalar[scalar$series_id == series_id, , drop = FALSE][1L, ]
  client <- nbs_client(
    transport = function(request) {
      body <- list(
        data = list(list(
          code = "202602MM",
          name = "2026-02",
          values = list(list(
            `_id` = scalar$indicator_uuid,
            value = "14749",
            du_name = scalar$unit_zh,
            da = scalar$geography_code,
            da_name = scalar$geography_en,
            catalogid = scalar$catalog_uuid
          ))
        )),
        success = TRUE,
        state = 20000,
        message = "success"
      )
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = jsonlite::toJSON(body, auto_unbox = TRUE)
      )
    },
    retries = 0L,
    now = fixed_now
  )
  fetched <- nbs_fetch_dataset(
    dataset_id,
    filters,
    start = "2026-02",
    end = "2026-02",
    client = client
  )
  expect_equal(fetched$route, descriptor$route)
  expect_equal(fetched$route, "portal_uuid_series")
  expect_equal(fetched$release_id, descriptor$release_id)
  expect_equal(fetched$family, descriptor$family)
})

test_that("household dataset routes quarterly migrant leaves to the portal", {
  dataset_id <- "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION"
  migrant_ids <- c(
    "NBS/CN/Q/MIGRANT_WORKERS/AVERAGE_MONTHLY_INCOME/LEVEL",
    "NBS/CN/Q/MIGRANT_WORKERS/TOTAL/LEVEL"
  )
  expect_true(all(
    migrant_ids %in% readNBS:::.nbs_dataset_series_candidates(dataset_id)
  ))

  series_id <- migrant_ids[[2L]]
  filters <- list(series_id = series_id)
  descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
  scalar <- nbs_manifest()
  scalar <- scalar[scalar$series_id == series_id, , drop = FALSE][1L, ]
  client <- nbs_client(
    transport = function(request) {
      body <- list(
        data = list(list(
          code = "202602SS",
          name = "2026-Q2",
          values = list(list(
            `_id` = scalar$indicator_uuid,
            value = "19139",
            du_name = scalar$unit_zh,
            da = scalar$geography_code,
            da_name = scalar$geography_en,
            catalogid = scalar$catalog_uuid
          ))
        )),
        success = TRUE,
        state = 20000,
        message = "success"
      )
      list(
        status = 200L,
        headers = list(`content-type` = "application/json"),
        body = jsonlite::toJSON(body, auto_unbox = TRUE)
      )
    },
    retries = 0L,
    now = fixed_now
  )
  fetched <- nbs_fetch_dataset(
    dataset_id,
    filters,
    start = "2026-Q2",
    end = "2026-Q2",
    client = client
  )
  expect_equal(nrow(fetched), 1L)
  expect_equal(fetched$route, "portal_uuid_series")
  expect_equal(fetched$route, descriptor$route)
  expect_equal(fetched$source_vintage, "latest_revised_database")
  expect_equal(fetched$dimension_key, nbs_dimension_key(filters))
})

test_that("exact filters fail closed before or after parsing", {
  client <- make_document_client("press-industrial.html")
  expect_error(
    nbs_fetch_dataset(
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      filters = list(category = "total"),
      client = client
    ),
    class = "nbs_dataset_filter_error"
  )
  expect_error(
    nbs_fetch_dataset(
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      filters = list(
        category = "unknown",
        measure = "business_revenue_level",
        period_basis = "ytd"
      ),
      client = client
    ),
    class = "nbs_dataset_filter_error"
  )
  expect_error(
    nbs_fetch_dataset(
      "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE",
      filters = list(
        category = "total",
        measure = "business_revenue_level",
        period_basis = "end_period"
      ),
      client = client
    ),
    class = "nbs_dataset_filter_error"
  )
  expect_error(
    nbs_resolve_dataset_series(
      "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
      list(
        product_code = "p001",
        measure = "price_yuan",
        specification = "20mm_q235"
      )
    ),
    class = "nbs_dataset_filter_error"
  )

  retail <- make_document_client("press-retail.html")
  expect_error(
    nbs_fetch_dataset(
      "NBS/CN/M/RETAIL_SALES/DETAIL",
      filters = list(
        category_code = "of_which_online_retail_sales_of_goods",
        measure = "level",
        period_basis = "current"
      ),
      client = retail
    ),
    class = "nbs_dataset_filter_error"
  )
})

test_that("retail official SA history keeps separate January and February", {
  result <- nbs_fetch_dataset(
    "NBS/CN/M/RETAIL_SALES/DETAIL",
    filters = list(
      category_code = "total_retail_sales_of_consumer_goods",
      measure = "mom_percent_official_sa",
      period_basis = "current"
    ),
    start = "2025-06",
    end = "2026-06",
    client = make_document_client("press-retail.html")
  )
  expect_equal(nrow(result), 13L)
  expect_true(all(c("2026-01", "2026-02") %in% result$period))
  expect_equal(unique(result$seasonal_adjustment), "official_seasonally_adjusted")
  expect_equal(unique(result$period_policy), "all_periods")
  expect_equal(unique(result$aggregation_basis), "single_period")
})

test_that("all retail descriptors and fetched rows use authoritative units", {
  dataset_id <- "NBS/CN/M/RETAIL_SALES/DETAIL"
  tuples <- nbs_dataset_valid_tuples(dataset_id)
  filter_names <- setdiff(names(tuples), "dimension_key")
  fetched <- nbs_fetch_dataset(
    dataset_id,
    client = make_document_client("press-retail.html")
  )
  expect_equal(nrow(tuples), 103L)
  expect_setequal(unique(fetched$dimension_key), tuples$dimension_key)

  for (index in seq_len(nrow(tuples))) {
    filters <- as.list(tuples[index, filter_names, drop = FALSE])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters)
    expected_en <- if (identical(filters$measure, "level")) {
      "100 million yuan"
    } else {
      "Percent"
    }
    expected_zh <- if (identical(filters$measure, "level")) {
      "亿元"
    } else {
      "百分比"
    }
    info <- tuples$dimension_key[[index]]
    expect_equal(descriptor$unit_en, expected_en, info = info)
    expect_equal(descriptor$unit_zh, expected_zh, info = info)
    expect_equal(
      unique(fetched$unit[fetched$dimension_key == info]),
      descriptor$unit_en,
      info = info
    )
  }
})

test_that("activity official-SA scalar series parse the revision table", {
  expected_ids <- c(
    "NBS/CN/M/INDUSTRY/VALUE_ADDED/MOM_PERCENT_OFFICIAL_SA",
    "NBS/CN/M/FAI/TOTAL/MOM_PERCENT_OFFICIAL_SA",
    "NBS/CN/M/RETAIL/TOTAL/MOM_PERCENT_OFFICIAL_SA"
  )
  expect_true(all(expected_ids %in% nbs_catalogue()$series_id))
  for (series_id in expected_ids) {
    result <- nbs_fetch_series(
      series_id,
      "2025-06",
      "2026-06",
      client = make_document_client("press-activity-sa.html")
    )
    expect_equal(nrow(result), 13L, info = series_id)
    expect_true(all(c("2026-01", "2026-02") %in% result$period))
    expect_equal(
      unique(result$seasonal_adjustment),
      "official_seasonally_adjusted"
    )
    expect_equal(
      unique(result$source_vintage),
      "official_nbs_revision_snapshot_2026-07-15"
    )
  }
})

test_that("English catalogue metadata has no placeholders or CJK leakage", {
  catalogue <- nbs_catalogue("en")
  datasets <- nbs_dataset_catalogue("en")
  dimensions <- nbs_dataset_dimensions(language = "en")
  cjk <- "[\u3400-\u4DBF\u4E00-\u9FFF]"
  expect_false(any(grepl(cjk, catalogue$title_en, perl = TRUE)))
  expect_false(any(grepl(cjk, catalogue$unit_en, perl = TRUE)))
  expect_false(any(grepl(cjk, datasets$title_en, perl = TRUE)))
  native_migrant <- dimensions$dataset_id ==
    "NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024"
  expect_false(any(grepl(
    cjk, dimensions$label_en[!native_migrant], perl = TRUE
  )))
  expect_false(any(grepl(cjk, dimensions$unit_en, perl = TRUE)))
  expect_false(any(grepl("__placeholder__|placeholder", dimensions$value)))
  expect_false(any(grepl("^Indicator [0-9]+$", catalogue$title_en)))
  expect_false(any(grepl(
    "Official published unit|official product unit|varies|various",
    dimensions$unit_en,
    ignore.case = TRUE
  )))
})

test_that("all English dimension labels and default titles are display-ready", {
  dimensions <- nbs_dataset_dimensions(language = "en")
  datasets <- nbs_dataset_catalogue("en")
  machine_like <- function(text) {
    grepl(
      "\\b(yoy|ytd|mom)\\b|[[:alnum:]]+_[[:alnum:]_]+",
      text, ignore.case = TRUE, perl = TRUE
    ) |
      grepl(
        paste0(
          "(?:^| \u2014 )[[:upper:]][[:upper:]_]+/",
          "[[:upper:]][[:upper:]_]+(?:/|$)"
        ),
        text, perl = TRUE
      ) |
      grepl(
        "^[[:lower:]][[:lower:][:digit:] (),.'’&+%/#-]+$",
        text, perl = TRUE
      )
  }

  expect_false(any(machine_like(dimensions$label_en)))
  expect_identical(
    dimensions$label_en[
      dimensions$dataset_id ==
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" &
        dimensions$dimension == "measure" &
        dimensions$value == "business_revenue_level"
    ][[1L]],
    "Business revenue"
  )
  expect_identical(
    dimensions$label_en[
      dimensions$dataset_id ==
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" &
        dimensions$dimension == "measure" &
        dimensions$value == "business_revenue_yoy"
    ][[1L]],
    "Business revenue \u2014 year-on-year change"
  )
  expect_identical(
    dimensions$label_en[
      dimensions$dataset_id ==
        "NBS/CN/M/INDUSTRIAL_BENEFITS/BY_ENTERPRISE_TYPE" &
        dimensions$dimension == "period_basis" &
        dimensions$value == "ytd"
    ][[1L]],
    "Year to date"
  )

  # Existing standards, statistical acronyms and product codes are display
  # text, not machine labels, and must survive normalization verbatim.
  expect_true(all(c(
    "GB/T 4754-2017", "Headline CPI", "LNG",
    "Rebar (Φ16-25mm,HRB400)"
  ) %in% dimensions$label_en))

  for (dataset_id in datasets$dataset_id) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    filter_names <- setdiff(names(tuples), "dimension_key")
    filters <- as.list(tuples[1L, filter_names, drop = FALSE])
    descriptor <- nbs_resolve_dataset_series(dataset_id, filters, "en")
    members <- dimensions[dimensions$dataset_id == dataset_id, , drop = FALSE]
    expected_labels <- vapply(filter_names, function(name) {
      members$label_en[
        members$dimension == name & members$value == filters[[name]]
      ][[1L]]
    }, character(1L))
    expected <- paste(
      datasets$title_en[datasets$dataset_id == dataset_id],
      paste(expected_labels, collapse = " \u2014 "),
      sep = " \u2014 "
    )

    expect_identical(descriptor$title_en[[1L]], expected, info = dataset_id)
    expect_false(machine_like(descriptor$title_en[[1L]]), info = dataset_id)
  }
})

test_that("all dataset tuples have unique public English titles", {
  datasets <- nbs_dataset_catalogue("en")
  dimensions <- nbs_dataset_dimensions(language = "en")
  tuple_count <- 0L

  for (dataset_id in datasets$dataset_id) {
    tuples <- nbs_dataset_valid_tuples(dataset_id)
    filter_names <- setdiff(names(tuples), "dimension_key")
    members <- dimensions[dimensions$dataset_id == dataset_id, , drop = FALSE]
    labels <- lapply(filter_names, function(name) {
      candidates <- members[members$dimension == name, , drop = FALSE]
      candidates$label_en[match(tuples[[name]], candidates$value)]
    })
    expect_false(
      any(vapply(labels, anyNA, logical(1L))),
      info = dataset_id
    )
    member_titles <- do.call(
      paste, c(labels, list(sep = " — "))
    )
    titles <- paste(
      datasets$title_en[datasets$dataset_id == dataset_id],
      member_titles,
      sep = " — "
    )

    expect_identical(anyDuplicated(titles), 0L, info = dataset_id)
    tuple_count <- tuple_count + nrow(tuples)
  }

  expect_identical(tuple_count, 25138L)
})
