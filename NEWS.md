# readNBS 0.8.3

* Expands the package documentation with task-oriented guidance for semantic
  series, dimensional datasets, provenance, reporting calendars, caching and
  maintenance-only live operations.
* Adds a deterministic end-to-end workflow that discovers and retrieves three
  retained official monthly series and renders an attributed comparison chart
  with a compact `ggplot2` line example in the README and Getting Started
  article.
* Adds a generated coverage map for all 20 scalar-series families, an exact
  coverage-inspection workflow, and a dedicated article explaining historical
  starting points, bounded archives, open-ended routes and dimensional tuples.
* Standardizes the package author, maintainer and package-code copyright holder
  as Chartwell Macro across package metadata, licensing, citations, documentation and
  the generated site. The technically required maintainer address now uses the
  repository's GitHub noreply address; optional public surfaces display only
  Chartwell Macro. NBS source-data rights remain separate and unchanged.
* Adds CRAN-oriented package metadata and portable documentation/release
  checks. This development branch has not been submitted to CRAN and does not
  imply CRAN acceptance.
* Converts official CPI, PPI and purchasing-price same-month comparison
  indexes to familiar year-on-year percentage changes through an explicit,
  provenance-preserving index-minus-100 presentation.
* Omits an incomplete leading archive year when deriving monthly flows from
  cumulative year-to-date levels, rather than fabricating its missing
  January-February boundary or rejecting later complete history.

# readNBS 0.8.2

* Reads compressed retained archives entirely in memory, avoiding large
  temporary decompression files that could truncate histories on low-disk
  deployments.
* Indexes retained observations by release period once per archive, so long
  dimensional histories do not repeatedly scan the full retained table.

# readNBS 0.8.1

* Adds a package-owned history-view contract to every dimensional dataset.
  Chart-ready wrappers, exact source fragments and naturally complete datasets
  are distinguished explicitly, and source fragments name their reviewed
  continuous replacement where one exists.

# readNBS 0.8.0

* Adds continuous chart-ready portal panels for 2,358 industrial financial
  concepts across adjacent GB/T industry and registration-type eras. Exact
  titles and measurement semantics must match; values are never rebased,
  interpolated or backcast, and row-level classification/UUID provenance is
  retained.
* Adds 14 long annual population and demographic series, with total population
  from 1949 and the detailed population panel from 2000.
* Adds 20 monthly output histories for electricity, cement, plate glass, crude
  steel and steel products from 2010, including current, cumulative and
  year-on-year measures.
* Extends the continuous all-official-bases housing view to the published
  year-to-date-average comparison, while preserving every exact base-era
  selection separately.
* Renames the optional January-February equal split as a transparent accounting
  benchmark. The package prefers official NBS seasonally adjusted series where
  available and does not misdescribe the equal split as the RBA's preferred
  Chinese New Year method.

# readNBS 0.7.1

* Adds chart-ready annual wage histories for 94 national, major-region and
  industry concepts. Ordinary selections now span 13--16 official
  observations through 2025 instead of exposing one six-year classification
  fragment as the complete series; the exact source tables remain available.
* Adds 50 migrant-worker histories with at least nine official annual
  observations. Headline total, outbound and local worker counts span
  2008--2025, including the exact values published in report prose after the
  headline table was discontinued.
* Preserves every selected release URL and source hash, records the 2012 wage
  coverage break, and does not interpolate, rebase or synthetically backcast
  any longitudinal observation.
* Applies the continuous housing comparison-base coverage audit to virtual
  all-official-base tuples, including their exact January/February inceptions.

# readNBS 0.7.0

* Adds a continuous, unrebased housing-price change series across all four
  official comparison-base eras for the published month-on-month and
  year-on-year indexes. Exact 2010, 2015, 2020 and 2025 base-era selections
  remain available; fixed-base levels are never joined across incompatible
  bases.
* Completes the retained housing release archive with the four official
  January--April 2019 releases, removing the only gap in the monthly change
  history from January 2011 onward.

# readNBS 0.6.9

* Corrects the native migrant-worker archive's family boundary to 2008, the
  earliest observation explicitly printed in the retained 2012 report, while
  continuing to identify 2012 as the earliest bundled first-release report.

# readNBS 0.6.8

* Makes the 33-row dimensional dataset catalogue independent of the much
  larger scalar-series catalogue, while preserving its reviewed coverage
  bounds exactly.
* Validates only duplicated scalar identities for cross-slice consistency and
  overlap, and translates dimensional labels lazily per selected dataset.
* Resolves retained snapshot coverage for the requested exact tuple instead of
  materialising encoded keys for every observation in a large panel.
* Uses the same character-only CSV contract through a faster parser for the
  large retained press archives, materially reducing first-series latency.

# readNBS 0.6.6

* Uses the canonical first-of-year observation date for retained annual
  Yearbook datasets, so exact annual date windows return their published row.

# readNBS 0.6.5

* Aligns retained capacity-history descriptors with their exact archive
  provenance, canonicalizes percent units, and preserves row-level route and
  source-vintage identities in hybrid capacity results.

# readNBS 0.6.4

* Preserves authoritative canonical units from retained press observations,
  fills only genuinely missing units from localized or exact-tuple metadata,
  and rejects generic dimensional observation units.

# readNBS 0.6.3

* Guarantees the app-facing canonical provenance fields on every dimensional
  dataset result, including the localized `unit` field on retained Yearbook
  observations.

# readNBS 0.6.2

* Makes every public English dataset-member label and resolved dataset title
  human-readable and context-aware while preserving stable dimension values,
  keys and transport identities.
* Derives runtime provenance and HTTP user-agent versions from the installed
  package metadata so they remain synchronized with `DESCRIPTION`.
* Corrects four copied English titles in the legacy industrial registration
  panel so current assets, financial costs and interest expenses remain
  distinct while preserving their exact UUID identities.

# readNBS 0.6.1

* Makes all 414 industrial enterprise-count leaves chart-ready with explicit
  `Enterprises` / `家` units instead of the portal's generic `Unit` label.
* Gives every retained 2012–24 migrant-worker report cell a deterministic,
  source-faithful English table/row/header label and contextual unit while
  preserving its native Chinese label, exact cell identity and source hash.

# readNBS 0.6.0

* Extends exact industrial portal coverage to all five non-stitching
  registration and GB/T classification panels, comprising 8,120 verified UUID
  leaves.
* Adds detailed CPI and PPI guides, retained 2010 Yearbook bridges, 2010–21
  wage histories, 2011–12 legacy household tables, 2010–11 demographic
  communiqués and native 2012–24 migrant-worker report archives.
* Adds annual national capacity utilisation from 2006, early quarterly
  national headlines from 2017 Q4, and keeps both separate from the 2018 Q2+
  detailed industry panel.
* Adds continuous official seasonally adjusted revision histories for
  industrial output, fixed investment and retail from February 2011, retaining
  each selected official revision vintage.
* Pins every exact portal UUID inception independently and makes the historical
  coverage audit fail closed on unexplained post-2010 starts.
* Splits retained archives behind dataset-scoped lazy loaders so small offline
  histories no longer pay the cold-load cost of unrelated large archives.
* Adds all 210 retained 2014–19 ten-day production-market releases with exact
  product/specification/unit identities, verified source-row alignment and an
  explicit inception boundary for every basket-specific tuple.

# readNBS 0.5.0

* Extends the bundled first-release archive from isolated recent releases to
  hundreds of official NBS releases, with a reproducible official-site search
  and capture workflow for future maintenance.
* Adds explicit historical schema eras instead of positional stitching:
  2010-, 2015-, 2020- and 2025-base housing panels; fixed-base versus year-to-date
  average housing comparisons; the 2012 15–59 and later 16–59 demographic age
  bands; and exact ten-day product name/specification/unit identities across
  basket changes.
* Parses legacy industrial headers and malformed retail HTML semantically,
  adds industrial-capacity first releases from 2018 Q2 before the portal's
  2021 Q1 boundary, and preserves actual observation years in wage tables.
* Uses the authoritative Chinese retail releases for the five 2023 months whose
  English NBS pages duplicate July, with bilingual semantic row parsing.
* Moves verified retail and industrial exact leaves to a data-driven portal
  map, retaining long revised histories while unmapped tuples continue to use
  release snapshots.
* Keeps genuinely incompatible series separate: the pre-Northeast three-region
  property panel is retained under explicit legacy region codes rather than
  joined to current definitions; old migrant-report hashed table selectors are
  not stitched.
* Makes archive generation fail closed when a download, parse, cardinality or
  target-to-index coverage check is incomplete.

# readNBS 0.4.0

* Replaces one-release dimensional dataset coverage with a historical hybrid:
  exact leaves use long revised portal histories where an equivalent UUID is
  verified, while genuinely press-only cells use a bundled normalized
  first-release archive.
* Adds release-level storage and schema versions, source URLs, basket and index
  bases, expected-cell counts and archive hashes. Dataset fetches are now
  deterministic and do not crawl historical pages at runtime.
* Extends detailed retail and national industrial-total leaves back through
  their matching scalar portal series, while preserving exact dimensional
  identity in returned metadata.
* Adds historical release coverage for industrial benefits, 70-city housing,
  regional property, detailed retail, households, annual demography and wages,
  and the current 2026 ten-day product basket where schemas are compatible.
* Handles February YTD-only retail tables, full-width household brackets,
  published industrial missing cells, and January housing tables with only two
  comparison columns.
* Keeps 2020- and 2025-base housing indices separate and refuses to stitch old
  ten-day product codes across official basket changes.

# readNBS 0.3.2

* Made exact dataset descriptors independent of caller filter ordering by
  canonicalising filters to each dataset's declared dimension order.
* Attached authoritative units to every exact retail-release tuple.

# readNBS 0.3.1

* Corrected pinned press-release dates to the publication timestamps printed
  on the official NBS source pages.
* Routed the two quarterly migrant-worker portal leaves advertised by the
  household dataset through the exact UUID-series transport.

# readNBS 0.3.0

* Expands the local catalogue to 508 stable scalar series backed by 515
  versioned transport slices, with reconciled bilingual metadata for 441 new
  official portal UUID leaves.
* Adds 12 stable dimensional dataset IDs for industrial enterprise financials,
  70-city housing prices, capacity utilisation, property, retail, demography,
  households, wages, migrant workers and ten-day production-market prices.
* Adds exact, fail-closed dimension resolution and fetches. Pinned
  press-release parsers validate complete expected panel cardinality before
  applying filters.
* Adds `nbs_dataset_valid_tuples()` so applications can construct dependent
  selectors from published combinations rather than dimension Cartesian
  products.
* Adds three official NBS seasonally adjusted activity month-on-month series
  from the published revision table.
* Replaces ambiguous January omission aliases with explicit reporting-calendar
  metadata and reference windows.
* Adds opt-in monthly regularisation for additive cumulative flows. The default
  is the RBA benchmark equal split; an IMF-style, caller-supplied working-day
  sensitivity is available, while rates, indexes and stocks are rejected.
* Adds deterministic, minimal, attributed HTML fixtures for every supported
  press-table schema.

# readNBS 0.2.0

* Expands the reviewed local catalogue from 3 to 72 stable national series
  covering prices, activity, property, labour, energy, PMI, GDP and household
  releases.
* Adds explicit `omit_january` and `omit_january_february`
  observation-calendar semantics for cumulative and current-period monthly
  activity series.
* Bumps the bundled manifest to `2026-07-30.3`.

# readNBS 0.1.1

* Defaults open-ended fetches to the most recently completed calendar period.
* Fails closed when an official response omits a requested period.
* Documents the 2015 quarterly GDP release-accounting boundary and bumps the
  bundled manifest to `2026-07-30.2`.

# readNBS 0.1.0

* Adds a defensive client for the current NBS National Data portal JSON flow.
* Adds stable sentinel series for headline monthly CPI, nominal current-quarter
  GDP, and year-end total population.
* Bundles a versioned bilingual manifest, including the four official headline
  CPI catalogue slices.
* Adds bounded retries, timeouts and per-client LRU caching.
* Preserves native values, source units and raw-response SHA-256 provenance.
* Advertises January 1998 as the first headline CPI period; earlier portal
  rows are blank.
