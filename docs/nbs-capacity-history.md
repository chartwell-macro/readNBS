# NBS industrial capacity-utilisation history

This is a deliberately separate retained archive for the national history that
predates the existing detailed quarterly industry panel.

- `NBS/CN/A/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HISTORY` contains the
  annual national rate for 2006--22 (17 observations).
- `NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION/NATIONAL_HEADLINE_2017_2019`
  contains the three early national quarterly headlines: 2017 Q4, 2018 Q1 and
  2019 Q1.

NBS says it began regular quarterly publication in 2017 Q4 and simultaneously
published annual history since the survey began in 2006.  The annual values are
the exact labels in the retained official Figure 1 PNG.  That chart is the
source; it is not a modelled reconstruction.

The existing `NBS/CN/Q/INDUSTRIAL_CAPACITY_UTILISATION` archive remains the
detailed 19-industry panel from 2018 Q2.  These schemas must not be merged:
the early headline history contains neither the later industry breakdown nor a
year-to-date duplicate for Q1.

## Provenance

`inst/extdata/nbs-capacity-history-source-index.csv` identifies every retained
official raw capture and its SHA-256.  The release index supplies one source
hash per retained observation group.  Two early quarterly points are explicitly
marked `official_subsequent_release_comparison`: NBS's 2018 Q4 release reports
that Q4 was 2.0 pp below 2017 Q4 (76.0%, thus the published 2017 Q4 headline is
78.0%); the 2019 Q2 release reports it was 0.5 pp above 2019 Q1 (76.4%, thus
the published 2019 Q1 headline is 75.9%).  This is an exact official comparison
and is retained as such, rather than mislabelled as a first-release capture.

## Storage and performance

The normalized 20-row observation archive is gzip-compressed.  It is loaded
lazily by `.nbs_load_capacity_history_observations()` and then cached for the
R session.  The helper validates the archive SHA-256, all five raw captures and
the four release-level cardinalities before returning data.  The benchmark in
the build handoff measures this cold path from a fresh package load; it is not
the later full-app catalogue start-up cost.

## Rebuild

Run the deterministic, offline builder from the package root:

```r
Rscript --vanilla scripts/build_nbs_capacity_history.R
```

It reads only the retained official files and re-emits the gzip archive and its
indexes.  It does not download data.
