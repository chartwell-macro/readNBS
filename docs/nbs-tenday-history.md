# NBS ten-day production-market-price history

This retained official snapshot covers the NBS 50-product production-market
price release from its January 2014 inception through December 2019.  It has
210 releases and 32,130 published cells (50 product tuples × three published
measures, plus three official up/down/flat counts, for each release).

The historical archive is loaded separately but is exposed through the same
ten-day dataset. It contains 59 distinct exact source tuples across the 2014--19
basket eras. Each identifier is the published product name, specification and
physical unit rather than an ordinal table row. That prevents a basket or
specification change being silently linked to a later product code.

The source pages contain nine category-heading rows interleaved with the 50
products. The builder aligns only numeric price rows to non-heading rows in the
official specification table and fails if their normalized product labels do
not match. Category headings can therefore never become fake products.

## Official calendar, including gaps

NBS did not publish the following collection windows.  They are deliberate
gaps, not missing observations and are never interpolated:

- 2015-02-D2
- 2016-02-D1
- 2017-01-D3
- 2018-02-D2
- 2019-02-D1
- 2019-05-D2

The retained annual release counts are 36 (2014), 35 (2015), 35 (2016), 35
(2017), 35 (2018) and 34 (2019).  Each frozen source page is retained under
`inst/extdata/nbs-tenday-history/`; the release index carries its official URL,
publication date, exact source SHA-256 and the archive SHA-256.

## Storage and verification

`nbs-tenday-history-observations.csv.gz` is a normalized gzip snapshot.  The
runtime helper checks the archive SHA, official release calendar, source-tuple
uniqueness and strict 153-cell cardinality per release before returning data.
The offline regression suite separately verifies all 210 raw-source hashes;
runtime reads do not re-hash 128 MB of provenance captures. A cell is accepted
only if its source page contains the 50 published tuples and three direction
counts.

## Rebuild

First discover/download the official pages (the script validates that NBS did
not return a JavaScript challenge instead of a table), then run the offline
builder:

```r
Rscript --vanilla scripts/discover_nbs_tenday_history.R targets.csv inst/extdata/nbs-tenday-history
Rscript --vanilla scripts/build_nbs_tenday_history.R targets.csv inst/extdata/nbs-tenday-history inst/extdata
```

The discovery script defaults to sequential retrieval.  A maintainer can set
`READNBS_TENDAY_DOWNLOAD_CONCURRENCY`, but the builder is deterministic and
never makes network requests.
