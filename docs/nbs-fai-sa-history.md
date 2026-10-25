# NBS fixed-asset-investment official SA month-on-month history

NBS states that it began publishing seasonally adjusted month-on-month data for
fixed-asset investment (excluding rural households) in April 2011. The earliest
retained official annual revision table, released on 17 January 2012, publishes
February--December 2011. No 2010 values are retained because the official
publication regime had not begun.

The archive retains one official annual revision table for every calendar year
2011--25. `nbs-fai-sa-history-vintages.csv.gz` holds all 179 source values;
`nbs-fai-sa-history-selected.csv.gz` selects a non-overlapping, continuous
2011-02--2025-05 series (172 values). The selected archive stops before the
existing June 2025 press snapshot, and every row retains its actual revision
vintage and source capture SHA-256.

The builder is deterministic and offline by default:

```sh
Rscript --vanilla scripts/build_nbs_fai_sa_history.R
```

Use `--download` only to obtain a missing official capture. The release index
contains all source URLs, dates, table cardinalities, selection boundaries and
hashes. Values are published revised snapshots, never labelled as first release.
