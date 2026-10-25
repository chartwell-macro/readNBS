test_that("household real-growth parser follows labels across legacy layouts", {
  table <- data.frame(
    item = c(
      "Table title", "Item", "Total Per Capita Income Nationwide",
      "Grouped by Permanent Residence", "Urban Households", "Rural Households",
      "Total Per Capita Income Nationwide Median", "Urban Households",
      "Rural Households", "Total Per Capita Expenditure Nationwide",
      "Grouped by Permanent Residence", "Urban Households", "Rural Households"
    ),
    level = c("", "", "100", "", "90", "70", "80", "75", "60", "80", "", "72", "58"),
    growth = c("", "", "8.8 (6.1)", "", "7.9 (5.4)", "9.2 (6.4)",
      "9.0", "7.6", "10.0", "8.3 (5.7)", "", "7.2 (4.7)", "9.5 (6.7)"),
    stringsAsFactors = FALSE
  )

  parsed <- readNBS:::.nbs_parse_household_real_panel(
    list(table), "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION"
  )

  expect_equal(nrow(parsed), 6L)
  expect_equal(parsed$value, c(6.1, 5.4, 6.4, 5.7, 4.7, 6.7))
  expect_equal(parsed$residence, c(
    "national", "urban", "rural", "national", "urban", "rural"
  ))
})

test_that("household real-growth parser supports the Chinese archive", {
  table <- data.frame(
    item = c(
      "指标", "（一）全国居民人均可支配收入", "按常住地分：", "城镇居民",
      "农村居民", "（二）全国居民人均可支配收入中位数", "城镇居民",
      "农村居民", "（三）全国居民人均消费支出", "按常住地分：",
      "城镇居民", "农村居民"
    ),
    level = as.character(seq_len(12L)),
    growth = c(
      "", "8.8（6.1）", "", "7.9（5.4）", "9.2（6.4）", "9.0", "7.6",
      "10.0", "8.3（5.7）", "", "7.2（4.7）", "9.5（6.7）"
    ),
    stringsAsFactors = FALSE
  )

  parsed <- readNBS:::.nbs_parse_household_real_panel(
    list(table), "NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION"
  )

  expect_equal(parsed$value, c(6.1, 5.4, 6.4, 5.7, 4.7, 6.7))
  expect_equal(parsed$residence, c(
    "national", "urban", "rural", "national", "urban", "rural"
  ))
})

test_that("housing parser validates and labels the fixed-base era", {
  dataset_id <- "NBS/CN/M/HOUSING_PRICES/70_CITY"
  dimensions <- nbs_dataset_dimensions(dataset_id, "en")
  cities <- dimensions$label_en[dimensions$dimension == "city_code"]

  overall <- as.data.frame(matrix("", nrow = 41L, ncol = 8L),
    stringsAsFactors = FALSE
  )
  overall[4L, c(2L, 3L, 4L, 6L, 7L, 8L)] <- rep(
    c("M/M", "Y/Y", "Fixed Base"), 2L
  )
  overall[5L, c(4L, 8L)] <- "Year 2020=100"
  overall_rows <- 7:41
  overall[overall_rows, 1L] <- cities[1:35]
  overall[overall_rows, 5L] <- cities[36:70]
  overall[overall_rows, c(2L, 3L, 4L, 6L, 7L, 8L)] <- "100.0"

  sized <- as.data.frame(matrix("", nrow = 76L, ncol = 10L),
    stringsAsFactors = FALSE
  )
  sized[4L, 2:10] <- rep(c("M/M", "Y/Y", "Fixed Base"), 3L)
  sized[5L, c(4L, 7L, 10L)] <- "Year 2020=100"
  sized_rows <- 7:76
  sized[sized_rows, 1L] <- cities
  sized[sized_rows, 2:10] <- "100.0"

  parsed <- readNBS:::.nbs_parse_housing_panel(
    list(overall, overall, sized, sized), dataset_id, index_base = "2025"
  )

  expect_equal(nrow(parsed), 1680L)
  expect_setequal(unique(parsed$comparison), c("mom", "yoy", "fixed_base"))
  expect_identical(unique(parsed$index_base), "2020")
  expect_equal(length(unique(paste(
    parsed$city_code, parsed$market, parsed$size_band, parsed$comparison
  ))), 1680L)
})

test_that("demography parser keeps the official working-age break explicit", {
  page <- function(age_label, age_value, year) {
    xml2::read_html(paste0(
      "<html><body><p>全年出生人口 1000 万人；死亡人口 900 万人。</p>",
      "<table><tr><th>指标</th><th>年末数</th><th>比重%</th></tr>",
      "<tr><td>全国总人口</td><td>140000</td><td>100.0</td></tr>",
      "<tr><td>其中：城镇</td><td>90000</td><td>64.3</td></tr>",
      "<tr><td>", age_label, "</td><td>", age_value, "</td><td>60.0</td></tr>",
      "<tr><td>60周岁及以上</td><td>30000</td><td>21.4</td></tr></table>",
      "</body></html>"
    ))
  }
  parse <- function(label, value, year) {
    readNBS:::.nbs_parse_demography_panel(
      page(label, value, year), "NBS/CN/A/DEMOGRAPHY/COMMUNIQUE"
    )
  }

  old <- parse("15-59岁（含不满60周岁）", "93000", 2012)
  current <- parse("16-59岁（含不满60周岁）", "86000", 2025)

  expect_true("age_15_59_year_end" %in% old$measure)
  expect_false("age_16_59_year_end" %in% old$measure)
  expect_true("age_16_59_year_end" %in% current$measure)
  expect_false("age_15_59_year_end" %in% current$measure)
})

test_that("ten-day parser accepts the Chinese archive direction-count wording", {
  path <- testthat::test_path("fixtures", "press-tenday.html")
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- sub(
    "Prices of 17 products increased, 31 kinds decreased, and 2 kinds remained flat.",
    "17种产品价格上涨，31种下降，2种持平。",
    html,
    fixed = TRUE
  )
  manifest <- readNBS:::.nbs_load_dataset_manifest()
  dataset <- manifest[
    manifest$dataset_id == "NBS/CN/10D/PRODUCTION_MARKET_PRICES",
    ,
    drop = FALSE
  ]

  parsed <- readNBS:::.nbs_parse_press_release(
    html, dataset, data.frame(index_base = "", stringsAsFactors = FALSE)
  )

  summary <- parsed[parsed$product_code == "summary", , drop = FALSE]
  expect_equal(nrow(parsed), 153L)
  expect_equal(summary$value, c(17, 31, 2))
})

test_that("published industrial note cells remain explicit missing values", {
  values <- c("（注）", "（注1）", "（note1）", "（Note1）")
  parsed <- lapply(values, function(value) {
    readNBS:::.nbs_number_or_published_missing(value, "industrial test")
  })

  expect_true(all(vapply(parsed, function(value) is.na(value$value), logical(1))))
  expect_identical(vapply(parsed, `[[`, character(1), "raw"), values)
})

test_that("legacy property regions use non-stitching dimension codes", {
  investment <- as.data.frame(matrix("", nrow = 5L, ncol = 5L),
    stringsAsFactors = FALSE
  )
  investment[3:5, 1L] <- c("Eastern", "Central", "Western")
  investment[3:5, 2:5] <- "1"
  sales <- as.data.frame(matrix("", nrow = 6L, ncol = 5L),
    stringsAsFactors = FALSE
  )
  sales[4:6, 1L] <- c("Eastern", "Central", "Western")
  sales[4:6, 2:5] <- "1"

  parsed <- readNBS:::.nbs_parse_property_regional_panel(
    list(investment, investment, sales),
    "NBS/CN/M/PROPERTY_DEVELOPMENT/BY_REGION"
  )

  expect_equal(nrow(parsed), 24L)
  expect_setequal(unique(parsed$region), c(
    "east_legacy_three_region", "central_legacy_three_region",
    "west_legacy_three_region"
  ))
})

test_that("the 2023 wage registration transition keeps comparable aggregates", {
  simple_table <- function(entities, columns = 4L, header_rows = 2L) {
    table <- as.data.frame(matrix(
      "", nrow = header_rows + length(entities), ncol = columns
    ), stringsAsFactors = FALSE)
    rows <- seq.int(header_rows + 1L, nrow(table))
    table[rows, 1L] <- entities
    table[rows, 2:columns] <- "1"
    table
  }
  regions <- paste0("region_", seq_len(5L))
  non_private_industries <- paste0("industry_", seq_len(20L))
  private_industries <- paste0("industry_", seq_len(19L))
  occupation_industries <- paste0("industry_", seq_len(17L))
  registration_simple <- simple_table(c(
    "合　　计", "（一）内资", "国有", "其他内资",
    "（二）港澳台投资", "（三）外商投资"
  ), columns = 2L)
  registration_occupation <- simple_table(c(
    "合　　计", "（一）内资", "国有", "私营", "其他内资",
    "（二）港澳台投资", "（三）外商投资"
  ), columns = 7L, header_rows = 3L)
  tables <- list(
    simple_table(regions),
    simple_table(non_private_industries),
    registration_simple,
    simple_table(regions),
    simple_table(private_industries),
    simple_table(regions, columns = 7L, header_rows = 3L),
    simple_table(occupation_industries, columns = 7L, header_rows = 3L),
    registration_occupation
  )

  parsed <- readNBS:::.nbs_parse_wages_panel(
    tables, "NBS/CN/A/EMPLOYMENT_WAGES"
  )

  expect_equal(nrow(parsed), 307L)
  registration <- parsed$registration[
    parsed$registration != "not_applicable"
  ]
  expect_setequal(unique(registration), c(
    "total", "domestic_invested_enterprises",
    "enterprises_with_investment_from_hong_kong_macao_and_taiwan",
    "foreign_invested_enterprises"
  ))
  expect_false(any(registration %in% c("state_owned", "private", "other")))
})

test_that("retail parser accepts authoritative Chinese monthly tables", {
  labels <- c(
    "社会消费品零售总额", "其中：除汽车以外的消费品零售额",
    "其中：限额以上单位消费品零售额", "其中：实物商品网上零售额",
    "城镇", "乡村", "餐饮收入", "其中：限额以上单位餐饮收入",
    "商品零售", "其中：限额以上单位商品零售", "粮油、食品类",
    "饮料类", "烟酒类", "服装、鞋帽、针纺织品类", "化妆品类",
    "金银珠宝类", "日用品类", "体育、娱乐用品类",
    "家用电器和音像器材类", "中西药品类", "文化办公用品类",
    "家具类", "通讯器材类", "石油及制品类", "汽车类",
    "建筑及装潢材料类"
  )
  headline <- data.frame(
    label = c("指 标", "指 标", labels),
    current_level = c("10月", "绝对量", rep("1", length(labels))),
    current_growth = c("10月", "同比增长", rep("2", length(labels))),
    ytd_level = c("1—10月", "绝对量", rep("3", length(labels))),
    ytd_growth = c("1—10月", "同比增长", rep("4", length(labels))),
    stringsAsFactors = FALSE
  )
  revision <- data.frame(
    year = c("", "2023年", "2023年"),
    month = c("月份", "9月", "10月"),
    growth = c("环比增速（%）", "0.1", "0.2"),
    stringsAsFactors = FALSE
  )

  parsed <- readNBS:::.nbs_parse_retail_panel(
    list(headline, revision), "NBS/CN/M/RETAIL_SALES/DETAIL"
  )

  expect_equal(nrow(parsed), length(labels) * 4L + 2L)
  expect_equal(length(unique(parsed$category)), length(labels))
  sa <- parsed[parsed$measure == "mom_percent_official_sa", ]
  expect_equal(sa$observation_date, as.Date(c("2023-09-01", "2023-10-01")))
})
