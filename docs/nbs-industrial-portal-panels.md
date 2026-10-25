# Industrial portal-panel metadata

`scripts/generate_nbs_industrial_portal_panels.R` produces the exact NBS
industrial UUID panels, keeping each published registration-type and GB/T
industry-classification era separate. It never stitches a UUID across those
boundaries.

The NBS indicator metadata's published display title is retained in every
series manifest row. Its parenthesised unit is used as the authoritative
measure unit, because the portal `du` unit UUID is occasionally stale for
industrial employee measures. In particular, the 110 average-employee UUIDs
whose titles state `10,000 persons` are emitted as `10,000 persons` / `万人`.

Use `--refresh-units` to update only those existing manifest and dimension
unit fields from the retained official display titles. It fails if any former
generic unit does not resolve to that published employee unit; it makes no
network request and does not alter UUID bindings or classification spans.
