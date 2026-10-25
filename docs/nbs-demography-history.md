# Early demographic communiqué history

`NBS/CN/A/DEMOGRAPHY/COMMUNIQUE` includes two retained early NBS releases in a
small, independently hashed snapshot archive. They are not extrapolations and
are not stitched into the later revised population database.

| Reference year | Exact published fields | Schema | Treatment |
| --- | --- | --- | --- |
| 2010 | total population at year end: 134,100 (10,000 persons) | `demography-total-population-preliminary-2010-v1` | The communiqué itself calls this a preliminary estimate and says final total/structure data would follow the Sixth Census release. No missing births, deaths, urbanisation or age values are inferred. |
| 2011 | total population 134,735; births 1,604; deaths 960; urbanisation 51.3%; age 15–59 94,072; age 60+ 18,499 (levels in 10,000 persons) | `demography-total-population-age-15-59-2011-v1` | The official English communiqué’s population table and population paragraph are frozen together. |

The source captures are bundled in `inst/extdata/nbs-demography-history/` and
verified by SHA-256 before the normalization generator writes the seven-cell
gzip archive. The 2010 capture SHA-256 is
`4e6bc7cbd0d2df2308e5932c113c2d671055b2dacfa675a581c2426e74a21709`; the
2011 capture SHA-256 is
`46690b985daf1bd9abc528798c3d097aa00d5ab86ad47fb2d63fe64facdd7205`.

Official sources: [2010 Chinese communiqué](https://www.stats.gov.cn/sj/tjgb/ndtjgb/qgndtjgb/202302/t20230206_1901954.html) and [2011 English communiqué](https://www.stats.gov.cn/english/PressRelease/201202/t20120222_72118.html).
