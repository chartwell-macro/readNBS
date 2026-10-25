## Test environments

* Local: macOS Sequoia 15.6, Apple silicon (`aarch64-apple-darwin24.4.0`),
  R 4.5.1, UTF-8 locale.
* Source archive: `readNBS_0.8.3.tar.gz`, 8,906,236 bytes, 240 archive
  members, SHA-256
  `e49b96c8f8e87ca1702c0415cb5a9b61318473dfd69c84d0def04b9b8c940ccc`.

The archive was produced with `R CMD build`, installed into a new isolated
library, and checked from the built archive. Raw maintainer source captures,
local check output, benchmarks, credentials, and development libraries are not
present in the archive. The package reference manual was also built separately
with `R CMD Rd2pdf`; it contains 36 pages.

The package source and built archive resolve the primary display metadata to
`Author: Chartwell Macro [aut, cre, cph]` and `Maintainer: Chartwell Macro`. The technically
required built `Maintainer` field uses the repository's GitHub noreply address.
The installed citation, README, package topic and pkgdown site display Chartwell Macro
without an email address. The root `CITATION.cff` supplies matching GitHub
metadata and is excluded from the source archive; `inst/CITATION` remains
installed.

On 2026-08-24, the case-insensitive name `readNBS` was absent from the current
CRAN source index, its exact CRAN archive URL returned HTTP 404, and the name
was absent from the Bioconductor release source index.

## R CMD check results

`R CMD check --as-cran --no-manual readNBS_0.8.3.tar.gz` completed with:

* 0 ERRORs
* 0 WARNINGs
* 1 NOTE

The complete fixture-backed test suite reported 38,274 passes, 0 failures,
0 warnings, and 0 skips. Examples, installed vignette files, and vignette
rebuilding passed. The slowest example took 1.472 seconds elapsed.

The local NOTE was:

1. CRAN incoming feasibility identified this as a new submission and reported
   nine HTTP 404 responses for the private GitHub repository, its issue and
   workflow links, files that are not yet on the default branch, the disabled
   GitHub Pages site, and the new Getting Started and Data coverage article
   links on that site.

This is recorded rather than suppressed. It must be rechecked after the
repository documentation is published and before any submission.

## Behaviour and downstream compatibility

The exported API remains 23 functions plus the registered
`print.nbs_client()` S3 method. A baseline/final offline probe covered 13
catalogue, fetch, cache, transformation, coverage, and failure-mode cases.
All 13 value digests and all 78 captured contract fields were identical,
including values, names and order, types, classes, attributes, dates,
conditions, warnings, requests, and side effects. The full historical
coverage audit also completed with all 36,278 rows covered.

The only intentional runtime portability change allows an installed source
archive to omit complete sets of repository-only raw captures. A source
checkout still verifies every retained capture SHA-256; a partial capture set
still fails; and both forms validate the normalized archives and index
relationships. The source-checkout capture test passed 11 expectations.
Two literal em dashes in generated labels are represented with the equivalent
Unicode escape; parsed strings, UTF-8 bytes, and observed output are identical.

Chartwell's complete focused provider contract suite was rerun read-only
against the isolated archive installation: 502/502 expectations passed with
no failure, warning, error, or skip. The local archive has no GitHub
`RemoteSha`; the focused test substitutes Chartwell's reviewed pin
`pre-publication baseline`, so this does not claim that a
local archive satisfies Chartwell's production SHA gate.

## Performance

The benchmark used an in-memory two-period CPI transport with no sockets,
100 warmups, 1,200 baseline repetitions, and two final runs of 1,200
repetitions. The aggregate 2,400-sample final comparison changed the
representative uncached fetch median/p95 by -16.4%/-15.6% and the cache-hit
median/p95 by -8.8%/-16.9%, within the stated 5% median and 10% p95 slowdown
budgets. Parsing and combined decode/parse also improved. JSON-only and
transport-shim medians were noisier (+13.4% and +22.9%), but remained below
0.04 ms and 0.01 ms respectively and did not produce a representative
workflow regression. Network time was measured separately as zero because
the benchmark deliberately used retained raw bytes.

## Submission status and blockers

No CRAN submission or release has been made.

Before submission, the maintainer must establish that the National Bureau of
Statistics of China material bundled in the package may be redistributed by
CRAN, or remove/replace it without violating the package's public contract.
The package-code copyright metadata identifies Chartwell Macro, but the package NOTICE
deliberately does not relicense or reassign the NBS-derived material. Its
source-data terms and redistribution permission remain a separate release
blocker, not a check exception.
