#!/usr/bin/env Rscript

# Pin the independently reviewable start date of every exact NBS portal UUID.
# Runtime coverage checks compare this file with the scalar manifest so a
# history truncation cannot silently redefine its own acceptable start.

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "extdata", "nbs-portal-inception-index.csv.gz")

pkgload::load_all(quiet = TRUE, export_all = FALSE)
manifest <- readNBS:::.nbs_load_manifest()
ids <- unique(manifest$series_id)
groups <- split(seq_len(nrow(manifest)), factor(manifest$series_id, levels = ids))

rows <- lapply(groups, function(indexes) {
  rows <- manifest[indexes, , drop = FALSE]
  earliest <- min(as.Date(rows$slice_start))
  first <- rows[as.Date(rows$slice_start) == earliest, , drop = FALSE]
  data.frame(
    manifest_version = "2026-08-11.1",
    series_id = rows$series_id[[1L]],
    required_from = format(earliest),
    classification_id = paste(unique(first$classification_id), collapse = "|"),
    source_url = paste(unique(first$source_url), collapse = "|"),
    authority = paste0(
      "Pinned exact official NBS portal UUID daterange; ",
      paste(unique(first$source_url), collapse = " | ")
    ),
    stringsAsFactors = FALSE
  )
})
index <- do.call(rbind, rows)
index <- index[order(index$series_id, method = "radix"), , drop = FALSE]
stopifnot(nrow(index) == length(ids), !anyDuplicated(index$series_id))

connection <- gzfile(output, "wt")
utils::write.csv(
  index, connection, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
close(connection)
message("Pinned ", nrow(index), " exact NBS portal UUID inception dates.")
