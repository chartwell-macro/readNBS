# NBS annual migrant-worker monitoring history

`NBS/CN/A/MIGRANT_WORKERS/HISTORY_2012_2024` is a source-faithful archive of
every numeric cell in the official NBS annual migrant-worker monitoring reports
for report years 2012--24.  It is deliberately separate from
`NBS/CN/A/MIGRANT_WORKERS`, which remains the detailed, single-schema 2025
dataset.

The archive preserves the original table geometry (`table_index`, `row_index`,
`column_index`, Chinese row label and raw value) and allocates one exact
release-cell measure selector per published number.  `schema_version` is also
an explicit selector.  This avoids presenting a later report's changed
classification, coverage or revised historical column as a backcast under the
2025 tuple system.

Each selector also has a complete English label comprising the translated
native table subject, row meaning and column/header path, followed by its exact
cell coordinates.  A finite, reviewed dictionary is used rather than automatic
transliteration; the builder fails if a new native token is unmapped or any CJK
text leaks into English metadata.  The corresponding `label_zh` retains the
native Chinese title/row/header path.  Where 2019--24 table titles and units sit
outside the HTML grid, the builder binds the deduplicated grid to its nearest
preceding native DOM title/unit paragraphs.

Units are resolved per cell from the native heading and layout.  Mixed tables
therefore distinguish population levels from rates, rate levels from percentage-
point changes, and monthly income/expense levels from growth.  Work-duration
cells use contextual units such as months per year, days per month and hours per
day; no selector falls back to a generic “published unit” label.

## Coverage and gaps

The retained source captures, release index and normalized archive cover the
following official report releases: 2012, 2013, 2014, 2015, 2016, 2017, 2018,
2019, 2020, 2021, 2022, 2023 and 2024.  Their exact URLs, release dates,
native-schema cell counts and SHA-256 hashes are in
`inst/extdata/nbs-migrant-history-release-index.csv`; raw HTML is in
`inst/extdata/nbs-migrant-history/`.

NBS states that it has published the annual monitoring report since 2011.  The
then-current direct official pages for the 2010 and 2011 report years could not
be located or retrieved during this archive build, so they are not represented
as first-release captures.  This is a release-provenance gap, not fabricated
data: later official tables do reproduce some earlier 2010--11 values, and the
archive retains those cells only with the later report's URL, raw table and
release period.  The same principle retains a small number of 2008--09
retrospective cells that appear in the 2012 report; they do not extend the
dataset's advertised 2010--24 monitoring-history coverage.

The number of native cells varies by report (306 in 2012, 276 in 2013, 465 in
2014, 296 in 2015, 242 in 2016, 214 in 2017, 145 in 2018, 142 in 2019--21,
and 139 in 2022--24).  These shifts are published schema changes, not missing
cells filled by the builder.

## Rebuild and validation

Run the deterministic, offline builder from the repository root:

```r
Rscript --vanilla scripts/build_nbs_migrant_history.R
```

The builder never downloads data.  It reads the retained official captures,
rejects non-numeric output cells, writes the normalized gzip archive and emits
the manifest, dimensions and release/source index.  The runtime helper in
`R/migrant_history.R` verifies the archive SHA-256, each raw capture hash and
each release's exact published-cell count before exposing any records.
