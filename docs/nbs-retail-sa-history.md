# NBS retail-sales official SA month-on-month history

NBS began publishing seasonally adjusted month-on-month retail-sales growth in
April 2011. The earliest retained official revision table, released 17 January
2012, supplies the source-published history from February 2011.

`scripts/build_nbs_retail_sa_history.R --download` captures 15 official NBS
release pages and writes three deliberately separate assets:

- `nbs-retail-sa-history-vintages.csv.gz` contains every value exactly as
  published in each retained revision table, with release/vintage date and
  capture SHA-256. These are revision snapshots, not first-release values.
- `nbs-retail-sa-history-selected.csv.gz` chooses non-overlapping published
  slices from 2011-02 through 2025-05. Its `selection_rule` makes that choice
  explicit and each row retains the source vintage.
- `nbs-retail-sa-history-release-index.csv` identifies source URL, published
  release date, table cardinality, selected interval and raw-capture hash for
  every snapshot.

The selected archive stops at May 2025 so it can join, without overlap, the
existing current press-revision-table snapshot beginning June 2025. Consumers
must not relabel selected values as first releases or silently overwrite them
with later revisions.
