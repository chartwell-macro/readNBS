# Pre-unified household-survey history

The integrated NBS household survey is a distinct quarterly system.  The
current `NBS/CN/Q/HOUSEHOLDS/INCOME_CONSUMPTION` dataset therefore starts at
its verified 2013-era inceptions and is never back-filled by relabelling the
older urban and rural surveys.

`readNBS` retains the two intervening annual Yearbook vintages as separate,
offline panels:

| Dataset | Exact measures | Years | Official sources |
| --- | ---: | --- | --- |
| `NBS/CN/A/HOUSEHOLDS/URBAN_LEGACY_2011_2012` | 29 | 2011–12 | [2012 Yearbook table 10-5](https://www.stats.gov.cn/sj/ndsj/2012/html/J1005C.HTM), [2013 Yearbook table 11-4](https://www.stats.gov.cn/sj/ndsj/2013/html/Z1104C.xls) |
| `NBS/CN/A/HOUSEHOLDS/RURAL_LEGACY_2011_2012` | 33 | 2011–12 | [2012 Yearbook table 10-18](https://www.stats.gov.cn/sj/ndsj/2012/html/J1018C.HTM), [2013 Yearbook table 11-17](https://www.stats.gov.cn/sj/ndsj/2013/html/Z1117C.xls) |

The urban panel contains survey composition, income components, expenditure,
eight consumption categories and their expenditure shares.  The rural panel
contains survey composition plus total, cash and net income, and total and
cash expenditure components.  Every raw table capture is bundled with its
SHA-256 in `nbs-households-legacy-source-index.csv`; the normalized 124-cell
archive is compressed separately and hash-checked at load time.

These measures have `urban_household_survey_pre2013` or
`rural_household_survey_pre2013` classification IDs.  They are useful history,
but not observations that can be concatenated to the 2013+ integrated survey.
