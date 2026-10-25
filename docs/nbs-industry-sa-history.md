# NBS industrial value-added official SA month-on-month history

NBS began publishing seasonally adjusted month-on-month industrial value-added
growth in April 2011. The earliest retained official revision table (released
9 March 2012) supplies the source-published history from February 2011.

`scripts/build_nbs_industry_sa_history.R --download` captures 15 official NBS
release pages and writes three deliberately separate assets:

- `nbs-industry-sa-history-vintages.csv.gz` contains every value as published
  in every retained revision table, with the release/vintage date and capture
  SHA-256. These are revision snapshots, not first-release observations.
- `nbs-industry-sa-history-selected.csv.gz` chooses non-overlapping published
  slices, one per snapshot, from 2011-02 through 2025-05. The choice is
  explicit in `selection_rule` and preserves the source vintage for each row.
- `nbs-industry-sa-history-release-index.csv` identifies the source URL,
  published release date, table cardinality, selected interval and raw-capture
  hash for every snapshot.

The selected archive stops at May 2025. It is designed to join, without an
overlap, the existing press-revision-table snapshot beginning June 2025.
Consumers must not relabel the selected values as first release or silently
replace them with later revisions.
