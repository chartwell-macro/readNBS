#' Access Data from China's National Bureau of Statistics
#'
#' `readNBS` is a defensive client for selected official time series and
#' release tables from the National Bureau of Statistics of China (NBS). It
#' gives R users a documented semantic layer over the public National Data
#' portal and a versioned, provenance-preserving view of the historical
#' release material bundled with the package.
#' The package is authored and maintained by Chartwell Macro.
#'
#' The package is intended for analysts, researchers, and applications that
#' need reproducible Chinese economic data without silently changing the
#' reporting geometry published by NBS. It is not a general-purpose scraper,
#' and it does not claim that every NBS table or series is covered.
#'
#' @section Source families:
#'
#' The public data contract combines several source families:
#'
#' * portal-backed scalar series identified by stable semantic IDs and mapped
#'   to the portal's versioned UUIDs;
#' * dimensional datasets and release panels whose published combinations are
#'   resolved through bundled dimension and validity manifests;
#' * bundled, hash-verified normalized observations from NBS press releases,
#'   Yearbook bridges, and retained native-report histories; and
#' * selected official seasonally adjusted revision histories and longitudinal
#'   collections where the complete title, unit, and measurement semantics
#'   support a defensible join.
#'
#' Source-table boundaries, classification eras, release vintages, and
#' missing or suppressed observations remain explicit. Values are not
#' interpolated, rebased, backcast, or silently converted into a different
#' reporting concept.
#'
#' @section Identity and provenance:
#'
#' Semantic series and dataset IDs are the supported user-facing identities;
#' portal UUIDs, catalogue records, schemas, and classification vintages are
#' transport and source details held behind the package manifests. Returned
#' observations retain the relevant source URL, route, source and manifest
#' metadata, retrieval time, and raw or archive hash fields where available.
#' Reference periods, release dates, vintage dates, and retrieval timestamps
#' are distinct concepts. See [nbs_manifest()], [nbs_catalogue()], and
#' [nbs_dataset_catalogue()] for the local metadata contract.
#'
#' @section Data coverage:
#'
#' [nbs_catalogue()] gives one row per supported scalar-series identity, while
#' [nbs_dataset_catalogue()] gives one row per dimensional dataset and
#' [nbs_dataset_valid_tuples()] enumerates its exact published combinations.
#' The `available_from` field is the earliest supported observation for an
#' identity. A missing `available_to` means that its declared route is
#' open-ended; a finite value identifies a bounded snapshot, release archive,
#' or classification era. Open-ended support does not imply that an
#' observation has already been published for every future period.
#'
#' The complete package coverage audit is available from
#' [nbs_historical_coverage()]. It reports one row per scalar series or exact
#' dimensional tuple and keeps the frequency, supported window, schema era,
#' policy, and authority together. Coverage is selected and versioned rather
#' than a claim to mirror the complete NBS portal. The Data coverage vignette
#' provides a subject-family map and historical starting points.
#'
#' @section Network, credentials, and caching:
#'
#' Loading the package, constructing an [nbs_client()], and using bundled
#' catalogues or [nbs_search()] do not make network requests. Portal-backed
#' fetches and live discovery operations use HTTPS by default and require
#' network availability; the portal routes are public and `readNBS` does not
#' manage credentials, authentication tokens, or credential storage. Bundled
#' press, Yearbook, and native-report observations are read locally, although
#' a dataset explicitly marked as a live document may opt into a live request.
#'
#' Portal responses are cached only in the client that made the request. The
#' cache has caller-controlled entry and time-to-live bounds and can be
#' cleared with [nbs_clear_cache()]. Local archive parsing also uses an
#' in-process cache. Ordinary package use does not write a persistent cache,
#' home-directory files, or global workspace state.
#'
#' @section Task map:
#'
#' * Find scalar series with [nbs_search()] and [nbs_catalogue()]; inspect
#'   panels with [nbs_dataset_catalogue()], [nbs_dataset_dimensions()], and
#'   [nbs_dataset_valid_tuples()]; resolve an exact published combination with
#'   [nbs_resolve_dataset_series()].
#' * Fetch observations with [nbs_fetch_series()] or
#'   [nbs_fetch_dataset()], using [nbs_client()] when request settings or
#'   cache isolation must be explicit.
#' * Check coverage and source metadata with [nbs_historical_coverage()],
#'   [nbs_assert_historical_coverage()], and [nbs_manifest()]. Treat
#'   [nbs_regularise_monthly()] and
#'   [nbs_comparison_index_to_percent()] as explicit derived transformations,
#'   not as replacements for official observations.
#' * Use [nbs_tree()] and [nbs_indicators()] for opt-in live portal discovery.
#'   The [nbs_yearbook_discover()], [nbs_yearbook_routes()],
#'   [nbs_yearbook_targets()], [nbs_yearbook_parse_table()], and
#'   [nbs_yearbook_discover_live()] functions support maintenance and
#'   inspection of Yearbook routes; they are not a substitute for the bundled
#'   deterministic data path.
#'
#' For a first successful workflow, start with the package README and the
#' Getting Started vignette, then use the function reference for the relevant
#' task family.
#'
#' @section Status and limitations:
#'
#' Coverage is deliberately selected and fail-closed. The NBS National Data
#' portal exposes public but undocumented JSON routes; its UUIDs, schemas,
#' paths, catalogue contents, and classification mappings may change without
#' notice. Network-backed results therefore describe the current or revised
#' portal state, while bundled release observations preserve their labelled
#' source vintage. Availability, period geometry, and comparability must be
#' checked for the exact requested series or tuple.
#'
#' Regularisation helpers are opt-in analytical benchmarks and do not recover
#' an unobserved official monthly split. Yearbook discovery and table parsing
#' are maintenance-oriented surfaces and may depend on changed live documents.
#' See [nbs_historical_coverage()] and the troubleshooting vignette before
#' relying on a live route in an automated workflow.
#'
#' @section Attribution and rights boundary:
#'
#' **Package code: MIT, copyright Chartwell Macro.** The `readNBS` package-authored code
#' and prose documentation are licensed under the repository's MIT license.
#' That package license does not cover NBS-derived observations, source
#' captures, official publications, rendered official-source values, or other
#' third-party material.
#'
#' `readNBS` is an independent software package and is not affiliated with,
#' sponsored, or endorsed by the National Bureau of Statistics of China. NBS
#' names, websites, releases, and published data remain subject to the rights
#' and attribution requirements of their respective owners. Users who
#' reprint or quote data are responsible for following the applicable NBS
#' attribution terms and for checking the terms of any linked or bundled
#' third-party material. The package does not present its own semantic IDs as
#' NBS-issued API identifiers.
#'
#' @seealso
#' [nbs_search()], [nbs_fetch_series()], [nbs_fetch_dataset()],
#' [nbs_client()], [nbs_dataset_catalogue()]
#'
#' @author Chartwell Macro
#'
#' @docType package
#' @name readNBS-package
#' @aliases readNBS
#' @keywords package
"_PACKAGE"
