# readNBS

`readNBS` is a defensive R client for selected official series and release
tables from the National Bureau of Statistics of China (NBS).

The package owns stable bilingual semantic identifiers. NBS catalogue and
indicator UUIDs, press-table layouts and classification vintages are treated
as changeable transport details held behind versioned manifests.

```r
library(readNBS)

nbs_catalogue()
nbs_search("CPI")

cpi <- nbs_fetch_series(
  "NBS/CN/M/CPI/HEADLINE/YOY_INDEX",
  start = "2025-01",
  end = "2026-06"
)

nbs_dataset_catalogue()
nbs_dataset_dimensions("NBS/CN/M/HOUSING_PRICES/70_CITY")
nbs_dataset_valid_tuples("NBS/CN/A/EMPLOYMENT_WAGES")

beijing_new_homes <- nbs_fetch_dataset(
  "NBS/CN/M/HOUSING_PRICES/70_CITY",
  filters = list(
    city_code = "110000",
    market = "new",
    size_band = "all",
    comparison = "mom",
    index_base = "2025"
  )
)

ppi_coal <- nbs_fetch_dataset(
  "NBS/CN/M/PPI/PRICE_DETAIL",
  filters = list(
    industry = "mining_and_washing_of_coal",
    comparison = "yoy_index"
  ),
  start = "2025-01",
  end = "2026-06"
)
```

The reviewed catalogue contains 8,762 stable national scalar series: 8,759
exact portal series and three official seasonally adjusted revision-table
series. Thirty-five additional dataset IDs expose release panels and
chart-ready longitudinal collections whose
identity is dimensional rather than one UUID per observation. Exact leaves use
the portal's long revised history when a semantically identical UUID exists.
Press-only leaves use bundled, hash-verified normalized archives, so ordinary
fetches never crawl historical NBS press pages. Coverage includes:

* CPI, PPI and industrial purchasing prices;
* industrial production, enterprise financials, profits and capacity use;
* services, detailed retail sales, fixed investment and energy output;
* national and regional property development, sales and funding;
* 70-city new and second-hand dwelling prices;
* manufacturing, non-manufacturing and composite PMI;
* GDP levels, growth indices and demand contributions;
* household income and consumption;
* population, employment and annual wage tables;
* the migrant-worker monitoring survey; and
* ten-day production-market prices for every official 50-product release from
  January 2014, with exact basket-specific product/specification/unit identity.

Historical coverage is evaluated per exact tuple, not inferred from the newest
release. Verified scalar-equivalent retail, property, industrial and household
leaves use the portal's longer revised database history. Depending on the exact
leaf, portal-backed retail begins as early as 1990, industrial totals in 1999,
property in 2000 and household measures in 2010. Incompatible classifications
remain separate datasets rather than being silently spliced.

The retained detail archives add the missing early release geometry:

* 2010 Yearbook bridges for demography, wages, property, households,
  industrial benefits and regional retail;
* demographic communiqués from 2010, 70-city housing from January 2011 and
  regional property from 2011;
* non-private and private wage tables from 2010–21, followed by the current
  wage release schemas;
* legacy urban and rural household tables for 2011–12, kept separate from the
  2013+ integrated survey;
* annual national capacity utilisation from 2006–22, early quarterly national
  headlines from 2017 Q4 and the detailed industry panel from 2018 Q2;
* native migrant-worker monitoring tables for every retained 2012–24 report,
  including explicitly labelled retrospective cells, plus the current report;
* 210 ten-day production-market releases for 2014–19 (32,130 cells and 59
  exact source tuples), joined to the current archive without stitching basket
  ordinals; and
* official seasonally adjusted industrial-output, fixed-investment and retail
  revision histories from their February 2011 published-history inception.

Source-table archive IDs preserve those defensible schema boundaries and are
not presented as continuous concepts. For ordinary charts, use
`NBS/CN/A/WAGES/LONGITUDINAL` and
`NBS/CN/A/MIGRANT_WORKERS/LONGITUDINAL`. The wage collection contains 94
national, major-region and industry series with 13--16 annual observations
through 2025. The migrant-worker collection contains 50 repeated indicators
with at least nine observed years; its headline total spans 2008--25. Values
are never interpolated, rebased or synthetically backcast, and every row keeps
the exact retained release URL, schema and source hash. The 2012 non-private
wage coverage break remains explicit in the descriptor and row provenance.

Use `nbs_catalogue()` for scalar IDs and `nbs_dataset_catalogue()` for panels.
`nbs_resolve_dataset_series()` resolves an exact dimension tuple locally and
fails closed if a combination is not published.
`nbs_dataset_valid_tuples()` supplies the authoritative non-Cartesian tuple
table for dependent selectors (315 published wage tuples, for example, rather
than the 52,920 combinations implied by independent member lists).

Industrial financial-history portal panels are explicitly separated by NBS
schema era: registration type through 2023 and from 2024, plus verified
GB/T 4754-2002 (through 2011), GB/T 4754-2011 (2012–17) and GB/T 4754-2017
(2018+) industry panels. Together they expose 8,120 exact UUID leaves. The
package does not infer category equivalence across classification or
registration breaks.

## Storage and cold loading

Portal series bundle only reviewed metadata and exact official UUIDs; their
observations are requested from NBS and cached in memory by the client. Press,
Yearbook and native-report histories are normalized into compressed CSV
archives, while retained official source captures and SHA-256 indexes provide
reproducible provenance. Dataset-scoped lazy loaders ensure that a small wage,
demography, capacity or migrant request does not inflate the multi-release
housing archive. Repeated local archive requests use the in-process parsed
cache; repeated portal requests use the client's bounded TTL cache.

`NBS/CN/M/PPI/PRICE_DETAIL` is an exact industry × comparison guide for the
producer price index. Its safe default tuple is
`industry = "mining_and_washing_of_coal"` with `comparison = "yoy_index"`.
Industry UUIDs are carried across eras only when the official concept matches;
the returned `classification_id` preserves the PPI era. A missing official leaf
(such as 2020–25 M/M “Mining of Other Ores”) is absent from valid tuples rather
than guessed.

## Reporting calendars and regularisation

Raw output preserves official NBS reporting geometry:

* cumulative monthly series omit January and put the January-February reading
  in February;
* current-month industry, retail and energy series omit January and February
  and begin in March; and
* quarterly cumulative household measures retain their year-to-date reference
  window.

The omission is therefore metadata, not a missing-value invitation. The
package does not silently duplicate, interpolate or split official values.
Every observation carries `reference_period_start`, `reference_period_end`,
`span_months`, `aggregation_basis` and `period_policy`.

`nbs_regularise_monthly()` is an explicit opt-in for additive cumulative flow
levels. It differences later year-to-date observations. Its default,
`method = "equal_split_benchmark"`, transparently assigns half of the reported
January-February total to each month. This is an accounting benchmark, not the
RBA's preferred treatment: the [RBA's study](https://www.rba.gov.au/publications/rdp/2015/2015-13/seasonally-adjusting-chinese-economic-time-series.html)
prefers explicit moving-holiday correction followed by seasonal adjustment
where the source observations allow it.
`method = "working_day_share"` is a separate, caller-calendar sensitivity based
on the approach used for one online-retail series in a [2025 IMF working
paper](https://meetings.imf.org/-/media/Files/Publications/WP/2025/English/wpiea2025217-source-pdf.ashx).
Both outputs are labelled derived; neither identifies the unobserved official
monthly split. Rates, indexes and stocks are rejected.

The wider lesson from the [RBA's study of Chinese seasonal
adjustment](https://www.rba.gov.au/publications/rdp/2015/2015-13/seasonally-adjusting-chinese-economic-time-series.html):
Chinese New Year and moving working-day effects need explicit regressors, and
ad hoc adjustment can materially distort the data. For series with separately
observed months, use a maintained Chinese holiday calendar and
X-13-ARIMA-SEATS-style, series-specific moving-holiday regressors. Do not claim
that such regressors identify January and February for a combined release: the
RBA could not estimate a Chinese New Year correction for industrial production
or fixed investment under that reporting constraint.
Where NBS publishes an official seasonally adjusted month-on-month series,
`readNBS` exposes it directly and labels it `official_seasonally_adjusted`.

## Long histories and exact source views

Chart-ready longitudinal datasets are separate from exact source tables and
base-era panels. They join observations only when the complete official title,
unit and measurement semantics agree across adjacent eras. No value is
interpolated, rebased or backcast, and every returned row keeps its source UUID,
classification/base, release URL and hash. Current longitudinal collections
cover wages, migrant workers, population, industrial financial indicators and
key industrial output. The 70-city housing dataset uses
`index_base = "all_official_bases"` for continuous published change measures;
an exact base remains selectable when a particular official vintage is needed.

## Why this package owns the semantic layer

The current open-source alternatives do not cover this contract. Python
`nbsdata` provides low-level UUID discovery and fetches (and currently requires
Python 3.14 or later), while `mbk-dev/nbsc` is a narrow headline-series
wrapper. Neither supplies stable bilingual IDs, versioned dimensional
press-release panels, first-release provenance, explicit reference-window
geometry or fail-closed panel cardinality. `readNBS` therefore uses direct
official routes rather than wrapping either client.

## Design guarantees

* No network request occurs when the package is loaded, when a client is
  created, or when the local catalogue is searched.
* Requests have bounded timeouts, retry counts and requested-period counts.
* The cache is per client, has an explicit maximum entry count and time to
  live, and may be cleared explicitly.
* Missing and suppressed tokens become `NA_real_`, never zero.
* Each response must match the exact requested indicator, catalogue and
  national geography UUIDs.
* Unexpected response shapes, period codes, duplicated periods and overlapping
  manifest slices fail explicitly.
* Bundled press snapshots validate each release's complete expected table
  cardinality before applying an exact tuple filter. Live archive crawling is
  restricted to the opt-in maintenance generator.
* Raw source observations are not transformed. In particular, a CPI value of
  `101.0` remains an index value. The portal currently emits `%` as its raw unit
  for that series; `readNBS` preserves that raw metadata separately while
  exposing the faithful semantic unit.
* Each observation contains retrieval time, transport UUIDs, source URL,
  manifest version, semantic series notes, and a SHA-256 hash of the raw
  response.
* Reference period, retrieval time, release date and vintage date are distinct
  fields. For portal/database observations the latter two are `NA` because the
  portal does not expose them, and `data_status` is `latest_revised`. Pinned
  press observations carry their explicit release and vintage dates.

## Configuration and testing

`nbs_client()` accepts separate English and Chinese portal bases, one or more
observation endpoint paths, timeout and retry bounds, cache limits, and a
transport function. A transport can therefore be injected for deterministic
tests without changing process-global state.

Archive maintenance is deliberately offline and reproducible. Run
`scripts/discover_nbs_press_archive.R` to freeze official release URLs and page
captures, then `scripts/update_nbs_press_archive.R` to regenerate the normalized
observations and release index. Both stages fail closed on unknown table shapes;
package users never crawl historical pages at runtime.

The defaults can also be hot-patched with:

* `READNBS_BASE_URL_EN`
* `READNBS_BASE_URL_ZH`
* `READNBS_OBSERVATION_PATH` (comma-separated paths are accepted)

The English or Chinese base is selected per request with `language = "en"` or
`language = "zh"`. Normal search uses the bundled manifest and does not call
the portal's global search endpoint.

## Sentinel reconciliation

The installed package's default transport was checked against the official
portal on 29 July 2026:

| Series | Portal value | Official release |
|---|---:|---|
| June 2026 headline CPI | `101.0` (same month previous year = 100) | [NBS reports 1.0% year-on-year](https://www.stats.gov.cn/english/PressRelease/202607/t20260710_1964094.html) |
| 2026 Q2 nominal current-quarter GDP | `361511.1` (100 million yuan) | [NBS release table reports 361511 at its published precision](https://www.stats.gov.cn/english/PressRelease/202607/t20260717_1964160.html) |
| 2025 year-end population | `140489` (10,000 persons) | [NBS 2025 statistical communiqué](https://www.stats.gov.cn/english/PressRelease/202602/t20260228_1962661.html) |

The GDP sentinel uses the portal's revised current-quarter history throughout.
NBS changed its quarterly release accounting from cumulative through 2015 Q2 to
current-quarter from 2015 Q3; that boundary is retained in catalogue and
observation provenance.

The GDP decimal difference is release-table rounding, not a semantic
discrepancy. These checks are evidence, not network-dependent package tests;
the package test suite is entirely fixture-based.

## Terms, attribution and limitations

NBS describes the rebuilt National Data database, launched on 27 March 2026,
as being in a trial period. Its public portal JSON endpoints are not a
documented or versioned developer API. Paths, schemas and UUIDs may change
without notice.

NBS terms state that statistical data may be downloaded and used, and require
prominent NBS and website attribution when data are reprinted or quoted:

<https://www.stats.gov.cn/english/nbs/200701/t20070104_59236.html>

Outputs carry:

> Source: National Bureau of Statistics of China | https://data.stats.gov.cn/

This package does not redistribute the full NBS catalogue or NBS branding. It
bundles attributed normalized published cells and the specific retained source
captures needed to reproduce historical tables and verify their hashes. Portal
history is current/revised; archived press-table rows preserve their labelled
release vintage and official source URL. It should not be described as a
supported NBS API client.

The headline CPI catalogue changes at official five-year classification
boundaries. Those four slices are stitched only because the bundled manifest
explicitly maps them to the same headline year-on-year index; each observation
retains its slice and classification identity. The returned `series_note`
explicitly discloses the January 2026 classification and base-period break. No
arbitrary catalogue series are joined.

The official portal returns blank monthly headline CPI observations before
January 1998. The bundled catalogue therefore advertises `1998-01-01` as the
first available period and uses it as the default history start.
