# Retail sales portal history

`NBS/CN/M/RETAIL_SALES/PORTAL_UNDERLYING` is an exact portal panel over the
underlying official retail release rows.  Every selectable tuple is a stable
`series_id` mapped to one reviewed NBS National Data UUID leaf; it is not a
synthetic total or a category inferred from text.

The panel includes national total, urban/rural, goods and catering, retail of
enterprises above the designated size, retail formats, online retail, and
designated-goods categories.  Its purpose is to make the series underlying the
headline retail release browseable without pretending that later category
introductions existed in 2010.

The raw official calendar is retained.  Current-period series have no separate
January or February observation, while cumulative series omit January and put
the combined January--February reading in February.  `nbs_regularise_monthly()`
is an explicit optional transformation; the panel itself never invents a
monthly observation.

Some leaves have earlier official history; `valid_from` and `valid_to` are
stored per member and the scalar descriptor is authoritative for each series'
classification/slice history.  Series are never stitched across an official
renaming or classification break.
