.nbs_portal_inception_cache <- new.env(parent = emptyenv())

.nbs_load_portal_inceptions <- function() {
  if (exists("index", envir = .nbs_portal_inception_cache, inherits = FALSE)) {
    return(get("index", envir = .nbs_portal_inception_cache, inherits = FALSE))
  }
  index <- utils::read.csv(
    .nbs_extdata_path("nbs-portal-inception-index.csv.gz"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  required <- c(
    "manifest_version", "series_id", "required_from", "classification_id",
    "source_url", "authority"
  )
  if (any(!required %in% names(index)) || !nrow(index) ||
      anyDuplicated(index$series_id) || any(!nzchar(index$series_id)) ||
      anyNA(suppressWarnings(as.Date(index$required_from))) ||
      any(!nzchar(index$source_url)) || any(!nzchar(index$authority))) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      "The pinned NBS portal-inception index is malformed."
    )
  }

  # The index is deliberately separate from the runtime manifest.  Requiring
  # exact agreement makes an accidental history truncation fail closed unless
  # the independently reviewed inception index is also regenerated.
  manifest <- .nbs_load_manifest()
  ids <- unique(manifest$series_id)
  earliest <- tapply(
    as.numeric(as.Date(manifest$slice_start)),
    factor(manifest$series_id, levels = ids),
    min
  )
  matched <- match(index$series_id, ids)
  indexed_start <- as.numeric(as.Date(index$required_from))
  if (anyNA(matched) || length(indexed_start) != length(ids) ||
      !setequal(index$series_id, ids) ||
      any(indexed_start != unname(earliest[matched]))) {
    .nbs_abort(
      "nbs_coverage_policy_error",
      paste(
        "The pinned NBS portal-inception index does not match the exact",
        "UUID manifest dateranges."
      )
    )
  }
  index$required_from <- as.Date(index$required_from)
  assign("index", index, envir = .nbs_portal_inception_cache)
  index
}

.nbs_portal_inception <- function(series_id) {
  if (!length(series_id)) return(NULL)
  index <- .nbs_load_portal_inceptions()
  matched <- match(as.character(series_id), index$series_id)
  data.frame(
    required_from = as.Date(ifelse(
      is.na(matched), NA_real_, as.numeric(index$required_from[matched])
    ), origin = "1970-01-01"),
    classification_id = ifelse(
      is.na(matched), "", index$classification_id[matched]
    ),
    source_url = ifelse(is.na(matched), "", index$source_url[matched]),
    authority = ifelse(is.na(matched), "", index$authority[matched]),
    stringsAsFactors = FALSE
  )
}
