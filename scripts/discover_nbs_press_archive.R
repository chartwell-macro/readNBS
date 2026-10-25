#!/usr/bin/env Rscript

# Offline maintenance discovery/downloader for official NBS releases. It uses
# the search endpoint linked from the NBS English site, then freezes an explicit
# target manifest and local page captures for update_nbs_press_archive.R.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 2:3) {
  stop(
    paste(
      "Usage: discover_nbs_press_archive.R OUTPUT_CSV PAGES_DIR",
      "[SEED_TARGETS_CSV]"
    ),
    call. = FALSE
  )
}
output_path <- normalizePath(args[[1L]], mustWork = FALSE)
pages_dir <- normalizePath(args[[2L]], mustWork = FALSE)
seed_path <- if (length(args) == 3L) {
  normalizePath(args[[3L]], mustWork = TRUE)
} else {
  ""
}
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

nbs_search <- function(query, page_size = 20L) {
  search_url <- paste0(
    "https://www.stats.gov.cn/search/english/s?qt=",
    utils::URLencode(query, reserved = TRUE)
  )
  html <- httr2::request(search_url) |>
    httr2::req_retry(max_tries = 4L) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  lines <- strsplit(html, "\n", fixed = TRUE)[[1L]]
  start <- grep("initPubProperty", lines, fixed = TRUE)[[1L]]
  values <- trimws(lines[(start + 1L):(start + 15L)])
  values <- sub(",$", "", values)
  values <- gsub(intToUtf8(39L), "", values, fixed = TRUE)

  request_page <- function(page) {
    response <- httr2::request("https://api.so-gov.cn/s") |>
      httr2::req_headers(suid = values[[14L]]) |>
      httr2::req_body_form(
        siteCode = values[[1L]],
        tab = values[[2L]],
        timestamp = values[[7L]],
        wordToken = values[[8L]],
        page = page,
        pageSize = page_size,
        qt = query,
        timeOption = 0L,
        sort = "dateDesc",
        keyPlace = 1L,
        fileType = ""
      ) |>
      httr2::req_retry(max_tries = 4L) |>
      httr2::req_perform()
    httr2::resp_body_json(response, simplifyVector = FALSE)$data$search
  }

  first <- request_page(1L)
  pages <- max(1L, ceiling((first$totalHits %||% 0L) / page_size))
  searches <- c(
    list(first),
    if (pages > 1L) lapply(seq.int(2L, pages), request_page) else list()
  )
  documents <- unlist(
    lapply(searches, function(x) x$searchs %||% list()),
    recursive = FALSE
  )
  if (!length(documents)) {
    return(data.frame(
      title = character(), href = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    title = vapply(documents, function(x) x$title %||% "", character(1L)),
    href = vapply(documents, function(x) x$viewUrl %||% "", character(1L)),
    stringsAsFactors = FALSE
  )
}

nbs_chinese_search <- function(
  query,
  page_size = 20L,
  start_date = "",
  end_date = "",
  key_place = 1L,
  sort = "dateDesc"
) {
  request_page <- function(page) {
    response <- httr2::request("https://api.so-gov.cn/query/s") |>
      httr2::req_body_form(
        siteCode = "bm36000002",
        page = page,
        pageSize = page_size,
        qt = query,
        timeOption = if (nzchar(start_date) && nzchar(end_date)) 2L else 0L,
        startDateStr = start_date,
        endDateStr = end_date,
        sort = sort,
        keyPlace = key_place,
        fileType = ""
      ) |>
      httr2::req_retry(max_tries = 4L) |>
      httr2::req_perform()
    httr2::resp_body_json(response, simplifyVector = FALSE)
  }

  first <- request_page(1L)
  pages <- max(1L, ceiling((first$totalHits %||% 0L) / page_size))
  responses <- c(
    list(first),
    if (pages > 1L) lapply(seq.int(2L, pages), request_page) else list()
  )
  documents <- unlist(
    lapply(responses, function(x) x$resultDocs %||% list()),
    recursive = FALSE
  )
  if (!length(documents)) {
    return(data.frame(
      title = character(), href = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    title = vapply(documents, function(x) x$data$titleO %||% "", character(1L)),
    href = vapply(documents, function(x) x$data$url %||% "", character(1L)),
    stringsAsFactors = FALSE
  )
}

ten_day_english_title <- function(title) {
  pattern <- paste0(
    "^([0-9]{4})年([0-9]{1,2})月([上中下])旬",
    "流通领域重要生产资料市场价格变动情况$"
  )
  parts <- regmatches(title, regexec(pattern, title, perl = TRUE))[[1L]]
  if (length(parts) != 4L) return(NA_character_)
  year <- as.integer(parts[[2L]])
  month <- as.integer(parts[[3L]])
  start_day <- c("上" = 1L, "中" = 11L, "下" = 21L)[[parts[[4L]]]]
  end_day <- if (start_day == 1L) {
    10L
  } else if (start_day == 11L) {
    20L
  } else {
    as.integer(format(
      seq(as.Date(sprintf("%04d-%02d-01", year, month)),
        by = "month", length.out = 2L
      )[[2L]] - 1,
      "%d"
    ))
  }
  sprintf(
    "Market Prices of Important Means of Production in Circulation, %s %d-%d, %d",
    month.name[[month]], start_day, end_day, year
  )
}

query_manifest <- data.frame(
  kind = c(
    "industrial", "industrial", "housing", "housing", "property",
    "property", "retail", "households", "households", "wages",
    "wages", "demography", "capacity", "capacity", "migrant",
    "migrant", "tenday"
  ),
  query = c(
    "Industrial Enterprises above the Designated Size",
    "Industrial Profits",
    "70 Medium and Large-sized Cities",
    "Commercial Residential Buildings in 70 Cities",
    "National Real Estate Development and Sales",
    "Investment in Real Estate Development",
    "Total Retail Sales of Consumer Goods",
    "Households Income and Consumption Expenditure",
    "Residents Income and Consumption Expenditure",
    "Average Annual Wages of Persons Employed in Urban Units",
    "Average Wage of Employed Persons in Urban Units",
    "STATISTICAL COMMUNIQUE OF THE PEOPLE REPUBLIC OF CHINA",
    "National Industrial Capacity Utilization Rate",
    "Industrial Capacity Utilization Rate",
    "Monitoring Survey Report on Migrant Workers",
    "Migrant Workers Monitoring Survey Report",
    "Market Prices of Important Means of Production in Circulation"
  ),
  stringsAsFactors = FALSE
)

discovered <- lapply(seq_len(nrow(query_manifest)), function(index) {
  result <- nbs_search(query_manifest$query[[index]])
  if (!nrow(result)) return(result)
  result$kind <- query_manifest$kind[[index]]
  result
})
discovered <- Filter(nrow, discovered)
discovered <- if (length(discovered)) {
  do.call(rbind, discovered)
} else {
  data.frame(
    title = character(), href = character(), kind = character(),
    stringsAsFactors = FALSE
  )
}

title_matches_kind <- function(title, kind) {
  lower <- tolower(title)
  switch(
    kind,
    industrial = grepl("profit", lower) && grepl("industrial enterprise", lower),
    housing = grepl("sales price", lower) && grepl("70", lower) &&
      grepl("cities", lower),
    property = grepl("real estate development", lower) &&
      !grepl("enterprises above", lower),
    retail = grepl("retail sales of consumer goods", lower),
    households = grepl("income", lower) && grepl("consumption", lower) &&
      grepl("household|resident", lower),
    wages = grepl("wage", lower) && grepl("urban unit", lower),
    demography = grepl("statistical communiqu", lower) &&
      grepl("economic and social development", lower),
    capacity = grepl("capacity utilization rate", lower) &&
      grepl("industrial", lower),
    migrant = grepl("migrant worker", lower) &&
      grepl("monitoring survey report|survey monitoring report", lower),
    tenday = grepl("market prices of important means of production", lower),
    FALSE
  )
}

if (nrow(discovered)) {
  keep <- mapply(title_matches_kind, discovered$title, discovered$kind)
  discovered <- discovered[keep, , drop = FALSE]
  discovered$href <- sub("^http://", "https://", discovered$href)
  discovered <- discovered[
    grepl("^https://www\\.stats\\.gov\\.cn/english/PressRelease/", discovered$href),
    ,
    drop = FALSE
  ]
}

supplemental <- data.frame(
  title = c(
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2018",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2018",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2018",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2019",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2020",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2020",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2021",
    "National Real Estate Development and Sales in the First Four Months of 2017",
    paste("Statistical Communique on National Economic and Social Development", 2012:2022),
    "Profits of Industrial Enterprises above Designated Size from January to February in 2019",
    "Profits of Industrial Enterprises above Designated Size from January to August in 2019",
    paste0(
      "Market Prices of Important Means of Production in Circulation, January 1-10, ",
      2020:2023
    ),
    "National Real Estate Development and Sales from January to March in 2009",
    "National Real Estate Development and Sales in 2011",
    "National Real Estate Development and Sales from January to May in 2012",
    "National Real Estate Development and Sales from January to February in 2013"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899996.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900111.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900206.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900601.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900685.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900971.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901053.html",
    "https://www.stats.gov.cn/english/PressRelease/201705/t20170517_1495113.html",
    "https://www.stats.gov.cn/sj/tjgb/ndtjgb/qgndtjgb/202302/t20230206_1901956.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898455.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1898704.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899041.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899428.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899855.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900241.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900640.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901004.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901393.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230228_1919011.html",
    "https://www.stats.gov.cn/english/PressRelease/201903/t20190328_1656767.html",
    "https://www.stats.gov.cn/english/PressRelease/201909/t20190927_1700067.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900591.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900961.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901332.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901706.html",
    "https://www.stats.gov.cn/english/NewsEvents/200904/t20090414_26132.html",
    "https://www.stats.gov.cn/english/PressRelease/201201/t20120119_72109.html",
    "https://www.stats.gov.cn/english/PressRelease/201206/t20120612_72159.html",
    "https://www.stats.gov.cn/english/PressRelease/201303/t20130311_72269.html"
  ),
  kind = c(
    rep("capacity", 7L), "property", rep("demography", 11L),
    rep("industrial", 2L), rep("tenday", 4L), rep("property", 4L)
  ),
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, supplemental)

# Known holes in the official search index. These exact pages are also useful
# sentinels for the release-gap audit because each sits inside an otherwise
# discoverable monthly run.
monthly_gap_supplements <- data.frame(
  title = c(
    "Profits of Industrial Enterprises above Designated Size from January to June in 2023",
    "Total Retail Sales of Consumer Goods in June 2023",
    "National Real Estate Development and Sales from January to July 2023",
    "National Real Estate Development and Sales from January to November 2023",
    "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in December 2023",
    "National Real Estate Development and Sales in the First Five Months of 2017"
  ),
  href = c(
    "https://www.stats.gov.cn/english/PressRelease/202308/t20230801_1941725.html",
    "https://www.stats.gov.cn/english/PressRelease/202307/t20230726_1941524.html",
    "https://www.stats.gov.cn/english/PressRelease/202308/t20230818_1942123.html",
    "https://www.stats.gov.cn/english/PressRelease/202312/t20231221_1945706.html",
    "https://www.stats.gov.cn/english/PressRelease/202401/t20240117_1946674.html",
    "https://www.stats.gov.cn/english/PressRelease/201706/t20170615_1503804.html"
  ),
  kind = c(
    "industrial", "retail", "property", "property", "housing", "property"
  ),
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, monthly_gap_supplements)

# The NBS English search index has a broad May-December 2023 hole even though
# the official release pages remain available. Freeze the missing run so a
# rebuild cannot silently regress to the partial search result.
monthly_2023_supplements <- data.frame(
  title = c(
    "The Profit of Industrial Enterprises above Designated Size from January to April in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to May in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to July in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to August in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to September in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to October in 2023",
    "The Profit of Industrial Enterprises above Designated Size from January to November in 2023",
    paste(
      "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in",
      month.name[5:11], "2023"
    ),
    "Investment in Real Estate Development for Jan-May 2023",
    "Investment in Real Estate Development for the First Half Year of 2023",
    "Investment in Real Estate Development for Jan-Aug 2023",
    "Investment in Real Estate Development for Jan-Sep 2023",
    "Investment in Real Estate Development for Jan-Oct 2023",
    "Investment in Real Estate Development in 2023",
    paste("Total Retail Sales of Consumer Goods in", c(
      "May", "July", "August", "September", "October", "November",
      "December"
    ), "2023")
  ),
  href = c(
    "https://www.stats.gov.cn/english/PressRelease/202305/t20230529_1940247.html",
    "https://www.stats.gov.cn/english/PressRelease/202306/t20230629_1940928.html",
    "https://www.stats.gov.cn/english/PressRelease/202309/t20230901_1942473.html",
    "https://www.stats.gov.cn/english/PressRelease/202310/t20231011_1943479.html",
    "https://www.stats.gov.cn/english/PressRelease/202311/t20231101_1944090.html",
    "https://www.stats.gov.cn/english/PressRelease/202311/t20231129_1944998.html",
    "https://www.stats.gov.cn/english/PressRelease/202401/t20240102_1946172.html",
    "https://www.stats.gov.cn/english/PressRelease/202306/t20230615_1940656.html",
    "https://www.stats.gov.cn/english/PressRelease/202307/t20230715_1941253.html",
    "https://www.stats.gov.cn/english/PressRelease/202308/t20230816_1942080.html",
    "https://www.stats.gov.cn/english/PressRelease/202309/t20230921_1943090.html",
    "https://www.stats.gov.cn/english/PressRelease/202310/t20231024_1943879.html",
    "https://www.stats.gov.cn/english/PressRelease/202311/t20231116_1944615.html",
    "https://www.stats.gov.cn/english/PressRelease/202312/t20231215_1945589.html",
    "https://www.stats.gov.cn/english/PressRelease/202306/t20230620_1940781.html",
    "https://www.stats.gov.cn/english/PressRelease/202307/t20230720_1941412.html",
    "https://www.stats.gov.cn/english/PressRelease/202309/t20230926_1943204.html",
    "https://www.stats.gov.cn/english/PressRelease/202310/t20231026_1943929.html",
    "https://www.stats.gov.cn/english/PressRelease/202311/t20231128_1944964.html",
    "https://www.stats.gov.cn/english/PressRelease/202402/t20240201_1947107.html",
    "https://www.stats.gov.cn/english/PressRelease/202306/t20230620_1940780.html",
    "https://www.stats.gov.cn/sj/zxfb/202308/t20230815_1941958.html",
    "https://www.stats.gov.cn/sj/zxfb/202309/t20230915_1942849.html",
    "https://www.stats.gov.cn/sj/zxfb/202310/t20231018_1943658.html",
    "https://www.stats.gov.cn/xxgk/sjfb/zxfb2020/202311/t20231115_1944537.html",
    "https://www.stats.gov.cn/sj/zxfb/202312/t20231215_1945575.html",
    "https://www.stats.gov.cn/english/PressRelease/202402/t20240201_1947119.html"
  ),
  kind = c(rep("industrial", 7L), rep("housing", 7L), rep("property", 6L),
    rep("retail", 7L)),
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, monthly_2023_supplements)

# The 2011--April 2019 housing archive is Chinese-only and the generic search
# ranking is incomplete for it. Search title fields year-by-year, retain exact
# official release pages, and translate the title solely for the deterministic
# period parser used by the offline archive builder.
housing_history_title <- function(title) {
  parts <- regmatches(title, regexec(
    "^([0-9]{4})年([0-9]{1,2})月份70个大中城市(?:商品)?住宅销售价格变动情况$",
    title, perl = TRUE
  ))[[1L]]
  if (length(parts) != 3L) return(NA_character_)
  sprintf(
    "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in %s %s",
    month.name[[as.integer(parts[[3L]])]], parts[[2L]]
  )
}
housing_history <- lapply(2011:2019, function(year) {
  found <- nbs_chinese_search(
    sprintf("%d年70个大中城市住宅销售价格变动情况", year),
    page_size = 100L, key_place = 2L, sort = "relevance"
  )
  if (!nrow(found)) return(found)
  exact <- grepl(
    sprintf("^%d年[0-9]{1,2}月份70个大中城市(?:商品)?住宅销售价格变动情况$", year),
    found$title, perl = TRUE
  )
  official <- grepl("^https://www\\.stats\\.gov\\.cn/", found$href)
  found <- found[exact & official, , drop = FALSE]
  if (!nrow(found)) return(found)
  found$title <- vapply(found$title, housing_history_title, character(1L))
  found$kind <- "housing"
  found
})
housing_history <- Filter(nrow, housing_history)
if (length(housing_history)) {
  discovered <- rbind(discovered, do.call(rbind, housing_history))
}

# The search index omits these three consecutive official releases.  Retain
# them explicitly so a fresh discovery is complete and reproducible.
housing_history_gap_supplement <- data.frame(
  title = c(
    "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in September 2016",
    "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in October 2016",
    "Sales Prices of Commercial Residential Buildings in 70 Medium and Large-sized Cities in November 2016"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899303.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899340.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899368.html"
  ),
  kind = "housing",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, housing_history_gap_supplement)

capacity_gap_supplements <- data.frame(
  title = c(
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2019",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2019",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2020",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2020"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900369.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900478.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900787.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900877.html"
  ),
  kind = "capacity",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, capacity_gap_supplements)

household_history_supplements <- data.frame(
  title = c(
    "Households' Income and Consumption Expenditure in the First Quarter of 2017",
    "Households' Income and Consumption Expenditure in the First Half of 2017",
    "Households' Income and Consumption Expenditure in the First Three Quarters of 2017",
    "Households' Income and Consumption Expenditure in 2017",
    "Households' Income and Consumption Expenditure in the First Quarter of 2018",
    "Households' Income and Consumption Expenditure in the First Half of 2018",
    "Households' Income and Consumption Expenditure in the First Three Quarters of 2018",
    "Households' Income and Consumption Expenditure in 2018"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899476.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899582.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899690.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899820.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899903.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899994.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900109.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900203.html"
  ),
  kind = "households",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, household_history_supplements)

household_gap_supplements <- data.frame(
  title = c(
    "Households' Income and Consumption Expenditure in the First Quarter of 2019",
    "Households' Income and Consumption Expenditure in the First Half of 2019",
    "Households' Income and Consumption Expenditure in the First Half of 2023",
    "Households' Income and Consumption Expenditure in the First Three Quarters of 2023",
    "Households' Income and Consumption Expenditure in 2023",
    "Households' Income and Consumption Expenditure in 2024"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900283.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1900368.html",
    "https://www.stats.gov.cn/english/PressRelease/202307/t20230726_1941522.html",
    "https://www.stats.gov.cn/english/PressRelease/202310/t20231026_1943920.html",
    "https://www.stats.gov.cn/english/PressRelease/202402/t20240201_1947120.html",
    "https://www.stats.gov.cn/sj/zxfb/202501/t20250117_1958325.html"
  ),
  kind = "households",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, household_gap_supplements)

wage_history_supplement <- data.frame(
  title = "Average Annual Wages of Persons Employed in Urban Units in 2023",
  href = paste0(
    "https://www.stats.gov.cn/xxgk/sjfb/zxfb2020/202405/",
    "t20240520_1950434.html"
  ),
  kind = "wages",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, wage_history_supplement)

# The maintained migrant-worker panel is intentionally limited to the current
# report because older hashed table selectors are not stable semantic IDs. The
# Chinese report is the authoritative source and is not returned by the English
# search endpoint, so keep this one supported release explicit.
migrant_current_supplement <- data.frame(
  title = "Migrant Workers Monitoring Survey Report 2025",
  href = paste0(
    "https://www.stats.gov.cn/sj/zxfb/202604/",
    "t20260430_1963472.html"
  ),
  kind = "migrant",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, migrant_current_supplement)

# The English search index does not return a complete quarterly capacity-
# utilisation history. Freeze the official Chinese release pages explicitly;
# keep English titles because update_nbs_press_archive.R derives the reference
# quarter from these labels.
capacity_quarters <- data.frame(
  title = c(
    "National Industrial Capacity Utilization Rate in the First Quarter of 2021",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2021",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2021",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2021",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2022",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2022",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2022",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2022",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2023",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2023",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2023",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2023",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2024",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2024",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2024",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2024",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2025",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2025",
    "National Industrial Capacity Utilization Rate in the Third Quarter of 2025",
    "National Industrial Capacity Utilization Rate in the Fourth Quarter of 2025",
    "National Industrial Capacity Utilization Rate in the First Quarter of 2026",
    "National Industrial Capacity Utilization Rate in the Second Quarter of 2026"
  ),
  href = c(
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901053.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901161.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901247.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901343.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901434.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901520.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901622.html",
    "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901716.html",
    "https://www.stats.gov.cn/sj/zxfb/202304/t20230418_1938713.html",
    "https://www.stats.gov.cn/sj/zxfb/202307/t20230715_1941275.html",
    "https://www.stats.gov.cn/sj/zxfb/202310/t20231018_1943652.html",
    "https://www.stats.gov.cn/sj/zxfb/202401/t20240116_1946617.html",
    "https://www.stats.gov.cn/sj/zxfb/202404/t20240416_1948583.html",
    "https://www.stats.gov.cn/sj/zxfb/202407/t20240715_1955616.html",
    "https://www.stats.gov.cn/sj/zxfb/202410/t20241018_1957036.html",
    "https://www.stats.gov.cn/sj/zxfb/202501/t20250117_1958324.html",
    "https://www.stats.gov.cn/sj/zxfb/202504/t20250416_1959314.html",
    "https://www.stats.gov.cn/sj/zxfbhjd/202507/t20250715_1960404.html",
    "https://www.stats.gov.cn/sj/zxfb/202510/t20251020_1961598.html",
    "https://www.stats.gov.cn/sj/zxfb/202601/t20260119_1962320.html",
    "https://www.stats.gov.cn/sj/zxfbhjd/202604/t20260416_1963322.html",
    "https://www.stats.gov.cn/sj/zxfbhjd/202607/t20260715_1964130.html"
  ),
  kind = "capacity",
  stringsAsFactors = FALSE
)
discovered <- rbind(discovered, capacity_quarters)

# The English index only exposes recent ten-day releases. The Chinese search
# endpoint retains the official release series; normalize its exact Chinese
# titles to the English form consumed by period_metadata(). Limit this archive
# to the current 50-product-basket history maintained by readNBS.
ten_day_archive <- nbs_chinese_search(
  "流通领域重要生产资料市场价格变动情况",
  start_date = "2020-01-01",
  # Late-month reference windows are published early in the next month.
  end_date = "2024-01-10"
)
if (nrow(ten_day_archive)) {
  ten_day_archive$title <- trimws(ten_day_archive$title)
  exact_title <- grepl(
    paste0(
      "^[0-9]{4}年[0-9]{1,2}月[上中下]旬",
      "流通领域重要生产资料市场价格变动情况$"
    ),
    ten_day_archive$title,
    perl = TRUE
  )
  official_page <- grepl(
    paste0(
      "^https://www\\.stats\\.gov\\.cn/sj/",
      "(?:zxfb|zxfbhjd)/[0-9]{6}/t[0-9]{8}_[0-9]+\\.html$"
    ),
    ten_day_archive$href,
    perl = TRUE
  )
  year <- suppressWarnings(as.integer(substr(ten_day_archive$title, 1L, 4L)))
  ten_day_archive <- ten_day_archive[
    exact_title & official_page & !is.na(year) & year >= 2020L,
    ,
    drop = FALSE
  ]
  if (nrow(ten_day_archive)) {
    ten_day_archive$title <- vapply(
      ten_day_archive$title,
      ten_day_english_title,
      character(1L)
    )
    ten_day_archive$kind <- "tenday"
    discovered <- rbind(
      discovered,
      ten_day_archive[c("title", "href", "kind")]
    )
  }
}

seed <- if (nzchar(seed_path)) {
  utils::read.csv(seed_path, stringsAsFactors = FALSE, check.names = FALSE)[
    c("title", "href", "kind")
  ]
} else {
  discovered[FALSE, , drop = FALSE]
}
targets <- rbind(seed, discovered[c("title", "href", "kind")])
targets$href <- sub("^http://", "https://", targets$href)
targets <- targets[
  nzchar(targets$title) & nzchar(targets$href) & nzchar(targets$kind),
  ,
  drop = FALSE
]
targets <- targets[!duplicated(paste(targets$kind, targets$href)), , drop = FALSE]
# These official pages are retained in the discovery notes above, but they do
# not publish the dimensional observations represented by this package: the
# 2020 communique omits the detailed population table and the 2009 property
# page predates the versioned national/three-region schemas. Exclude them from
# generation so the parser report represents unexpected failures only.
known_schema_omissions <- c(
  "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1901004.html",
  "https://www.stats.gov.cn/english/NewsEvents/200904/t20090414_26132.html",
  # The 2017 Q1 household table publishes nominal growth only; the dimensional
  # dataset intentionally contains the six bracketed real-growth leaves.
  "https://www.stats.gov.cn/sj/zxfb/202302/t20230203_1899476.html"
)
targets <- targets[!targets$href %in% known_schema_omissions, , drop = FALSE]
targets <- targets[order(targets$kind, targets$href), , drop = FALSE]

url_token <- function(url) {
  token <- regmatches(url, regexpr("t[0-9]{8}_[0-9]+", url, perl = TRUE))
  if (!length(token) || !nzchar(token)) {
    digest::digest(url, algo = "sha256", serialize = FALSE)
  } else {
    token
  }
}
targets$file <- file.path(
  pages_dir,
  paste0(targets$kind, "_", vapply(targets$href, url_token, character(1L)), ".html")
)

for (index in seq_len(nrow(targets))) {
  path <- targets$file[[index]]
  if (file.exists(path) && file.info(path)$size > 1000L) next
  response <- httr2::request(targets$href[[index]]) |>
    httr2::req_user_agent("readNBS archive maintainer/0.5.0") |>
    httr2::req_retry(max_tries = 5L) |>
    httr2::req_perform()
  writeBin(httr2::resp_body_raw(response), path)
}

targets$page <- "official-nbs-search"
targets <- targets[c("page", "title", "href", "kind", "file")]
utils::write.csv(targets, output_path, row.names = FALSE, na = "")
cat(sprintf("Wrote %d official release targets to %s.\n", nrow(targets), output_path))
