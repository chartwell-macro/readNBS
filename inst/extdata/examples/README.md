# Offline examples

These small files reproduce the payloads in the corresponding fixtures in
`tests/testthat/fixtures/` (the three JSON test fixtures have one redundant
empty line at EOF, which is not part of the JSON payload). They make the
package's documentation and examples deterministic after installation; the
examples use injected transports and do not contact the network.

| File | Source attribution and local provenance | Purpose |
| --- | --- | --- |
| `cpi-2026-june.json` | National Bureau of Statistics of China public data portal, English public-release observation flow (`https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/stream/esData`); the request is the June 2026 national CPI observation (`catalogid` `5c7452825c7c4dcba391db5ca7f335c5`, indicator `_id` `53180dfb9c14411ba4b762307c85920c`, period code `202606MM`). | Offline `nbs_fetch_series()` example. |
| `tree-monthly.json` | National Bureau of Statistics of China public data portal, English public-release catalogue-tree flow (`https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndexTreeAsync`), with the root query used by the package tests (`pid = ""`, `code = "1"`). | Offline `nbs_tree()` example. |
| `indicators-cpi-2026.json` | National Bureau of Statistics of China public data portal, English public-release indicator flow (`https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid`), for catalogue `5c7452825c7c4dcba391db5ca7f335c5`. | Offline `nbs_indicators()` example. |
| `yearbook-2011-left.html` | Minimal derived fixture from the National Bureau of Statistics of China 2011 Statistical Yearbook contents page (`https://www.stats.gov.cn/sj/ndsj/2011/left.htm`), retaining only the links needed to test route discovery. | Offline `nbs_yearbook_discover()`/`nbs_yearbook_routes()` example. |
| `yearbook-table-merged.html` | Minimal derived fixture representing the merged-header geometry and published values of 2011 Statistical Yearbook table `3-1` (`https://www.stats.gov.cn/sj/ndsj/2011/html/D0301C.HTM`). The retained source capture is recorded in `nbs-yearbook-snapshot-index.csv` with SHA-256 `bc132b67f3879c6c15af63f368e8219689b72428d592ef60fdc20c135c2246f4`. | Offline `nbs_yearbook_parse_table()` example with merged headers. |

The files are frozen test/example snapshots, not a claim that they are a
complete publication or a current copy of the source. They are source-derived
content; no additional copyright or licensing rights are asserted here. Check
the National Bureau of Statistics of China terms and the linked source before
redistributing or reusing the source content.
