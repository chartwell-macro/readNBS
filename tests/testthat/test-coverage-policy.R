test_that("historical coverage policy is explicit and narrowly scoped", {
  policy <- readNBS:::.nbs_load_coverage_policy()
  required_ids <- c(
      "capacity-scalar-current-catalogue", "capacity-panel-gbt4754-2017",
      "production-market-prices-inception",
      "integrated-households-scalar-inception",
      "integrated-households-panel-inception",
      "demography-births-2011-communique",
      "demography-deaths-2011-communique",
      "demography-urbanisation-2011-communique",
      "demography-age-15-59-2011-communique",
      "demography-age-60-plus-2011-communique",
      "demography-age-16-59-2013-definition",
      "housing-continuous-change-series-inception",
      "housing-continuous-headline-series-inception",
      "housing-continuous-ytd-average-inception",
      "housing-continuous-ytd-average-headline-inception",
      "wages-longitudinal-health-social-inception",
      "wages-longitudinal-household-services-inception",
      "wages-longitudinal-information-services-inception",
      "wages-longitudinal-public-management-inception"
  )
  expect_true(all(required_ids %in% policy$policy_id))
  expect_equal(anyDuplicated(policy$policy_id), 0L)
  expect_true(all(as.Date(policy$required_from) > as.Date("2010-01-01")))
  expect_true(all(nzchar(policy$authority)))
})

test_that("exact portal inceptions are independently pinned", {
  index <- readNBS:::.nbs_load_portal_inceptions()
  manifest <- readNBS:::.nbs_load_manifest()
  expect_setequal(index$series_id, unique(manifest$series_id))
  expect_equal(anyDuplicated(index$series_id), 0L)
  expect_true(all(nzchar(index$authority)))

  industrial <- index[startsWith(
    index$series_id, "NBS/CN/M/INDUSTRIAL_PORTAL/"
  ), , drop = FALSE]
  expect_equal(nrow(industrial), 8120L)
  expect_equal(min(industrial$required_from), as.Date("2010-02-01"))
})

test_that("coverage audit includes every scalar and exact published tuple", {
  audit <- nbs_historical_coverage()
  expected_scalars <- nrow(nbs_catalogue())
  expected_tuples <- sum(vapply(
    nbs_dataset_catalogue()$dataset_id,
    function(dataset_id) nrow(nbs_dataset_valid_tuples(dataset_id)),
    integer(1)
  ))
  expect_equal(nrow(audit), expected_scalars + expected_tuples)
  expect_equal(sum(audit$entity_type == "scalar"), expected_scalars)
  expect_equal(sum(audit$entity_type == "dataset"), expected_tuples)
  expect_false(anyNA(audit$available_from))
  expect_true(all(nzchar(audit$start_reason)))
  expect_true(all(nzchar(audit$schema_era)))
})

test_that("the 2010 baseline follows official monthly activity calendars", {
  expect_equal(
    readNBS:::.nbs_historical_baseline(
      "M", "current_january_february_not_separately_reported"
    ),
    as.Date("2010-03-01")
  )
  expect_equal(
    readNBS:::.nbs_historical_baseline(
      "M", "ytd_january_not_reported_february_combined"
    ),
    as.Date("2010-02-01")
  )
  expect_equal(
    readNBS:::.nbs_historical_baseline("M", "all_periods"),
    as.Date("2010-01-01")
  )
  expect_equal(
    readNBS:::.nbs_historical_baseline(
      c("M", "M"), c("omit_january", "omit_january_february")
    ),
    as.Date(c("2010-02-01", "2010-03-01"))
  )
})

test_that("authoritative inception dates are targets, not blanket waivers", {
  audit <- nbs_historical_coverage()
  capacity <- audit[
    audit$identity == "NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION", , drop = FALSE
  ]
  capacity_scalar <- audit[
    audit$entity_type == "scalar" &
      startsWith(audit$identity, "NBS/CN/Q/CAPACITY_UTILISATION/"), , drop = FALSE
  ]
  prices <- audit[
    audit$identity == "NBS/CN/10D/PRODUCTION_MARKET_PRICES", , drop = FALSE
  ]
  households <- audit[
    audit$identity == "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION" &
      grepl("residence=national", audit$dimension_key, fixed = TRUE), , drop = FALSE
  ]
  expect_true(all(capacity$required_from == as.Date("2018-04-01")))
  expect_true(all(capacity_scalar$required_from == as.Date("2021-01-01")))
  expect_equal(min(prices$required_from), as.Date("2014-01-01"))
  expect_equal(prices$available_from, prices$required_from)
  expect_true(all(
    prices$policy_id == "tenday-exact-tuple-inception-index"
  ))
  expect_true(all(households$required_from == as.Date("2013-01-01")))
  demography <- audit[
    audit$identity == "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE", , drop = FALSE
  ]
  expect_true(all(demography$required_from %in% as.Date(c(
    "2010-01-01", "2011-01-01", "2013-01-01"
  ))))
  expect_true(all(demography$status == "covered"))
  expect_true(all(capacity$status == "covered"))
  expect_true(all(capacity_scalar$status == "covered"))
  expect_true(all(prices$status == "covered"))
})

test_that("strict historical assertion is ready for completion CI", {
  audit <- nbs_historical_coverage()
  expect_true(all(audit$status == "covered"))
  expect_silent(nbs_assert_historical_coverage())
})
