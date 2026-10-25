# NBS annual wage history

`readNBS` retains the official Chinese NBS release tables for annual urban-unit
wages from 2010 through 2021 as two snapshot datasets:

- `NBS/CN/A/WAGES/NON_PRIVATE_HISTORY`
- `NBS/CN/A/WAGES/PRIVATE_HISTORY`

Each exact request selects `classification`, `table`, `region`, `industry`,
`registration`, and `measure`. This is intentional: a user must explicitly
choose the publication's industry-classification era rather than silently
stitching unlike series.

| Observation years | Classification selector | Scope |
| --- | --- | --- |
| 2010–11 | `*_employees_gbt4754_2002` | Early publications; non-private is explicitly *in-post employees*. |
| 2012–17 | `gbt4754_2011` | Published tables under GB/T 4754-2011. |
| 2018–21 | `gbt4754_2017` | Published tables under GB/T 4754-2017. |

The retained measures are the current-year average wage and the published
nominal growth rate. A release's preceding-year level is not stored as another
vintage of the same observation; the preceding year is retained from its own
first-release table. This gives one exact first-release observation per tuple
and year.

The 24 captured sources, their SHA-256 hashes, exact release dates and table
cell counts are in `inst/extdata/nbs-wages-history-release-index.csv`; raw NBS
HTML is retained under `inst/extdata/nbs-wages-history/`. The builder is
`scripts/build_nbs_wage_history.R` and is deterministic without network access
once those captures are present.

Known publication exceptions are represented rather than filled: the 2019
private release page contained an industry table but no regional table, and
private-sector registration tables are not published in this historical
release family. Provincial rows that occur in the 2010 non-private region table
remain source-table members.
