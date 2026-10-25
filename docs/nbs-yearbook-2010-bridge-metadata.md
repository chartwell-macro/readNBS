# 2010 Yearbook bridge metadata

The six retained 2011 National Bureau of Statistics Yearbook tables expose
their original Chinese row and header labels, including units which are often
held in a table-level unit row or a merged parent metric heading.  The offline
Yearbook builder freezes a reviewed English translation alongside a
source-faithful Chinese label and explicit unit for every one of the 450 2010
cells.

The translation and unit rules are kept in
`scripts/build_nbs_yearbook_archive.R`; rerunning the builder uses only the
retained table captures, refreshes the label map, observations, dimensions and
release-index archive files, and makes no network request.  A build fails if a
label or unit remains unclassified, so generic placeholder metadata cannot
enter the bridge datasets.

Published units represented in the archive are: yuan; 100 million yuan;
persons; 10,000 persons; square metres; 10,000 square metres; yuan per square
metre; percent; enterprises; beds; seats; vehicles; standard vehicles; handsets
per 100 persons; and sets per 100 households.
