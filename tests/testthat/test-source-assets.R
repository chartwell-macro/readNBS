test_that("source captures are fully hashed in checkout and absent from tarballs", {
  extdata_file <- function(relative) {
    source_path <- file.path("inst", "extdata", relative)
    if (file.exists(source_path)) return(source_path)
    installed_path <- system.file(
      "extdata", relative, package = "readNBS", mustWork = FALSE
    )
    if (nzchar(installed_path)) installed_path else source_path
  }

  expect_capture <- function(label, paths, expected) {
    paths <- as.character(paths)
    present <- file.exists(paths)
    if (all(present)) {
      actual <- vapply(paths, function(path) digest::digest(
        file = path, algo = "sha256", serialize = FALSE
      ), character(1L))
      expect_identical(unname(actual), unname(as.character(expected)), info = label)
    } else {
      # Raw captures are intentionally omitted from the source tarball. This
      # assertion is deliberately strict about partial sets: a source
      # checkout must hash every capture, while an installed package must
      # contain none of them.
      expect_false(any(present), info = label)
    }
  }

  # Each normalized archive and its release/source manifest is a runtime
  # contract and must survive source packaging.
  normalized <- c(
    "nbs-capacity-history-dataset-dimensions.csv",
    "nbs-capacity-history-dataset-manifest.csv",
    "nbs-capacity-history-observations.csv.gz",
    "nbs-capacity-history-release-index.csv",
    "nbs-capacity-history-source-index.csv",
    "nbs-demography-history-observations.csv.gz",
    "nbs-demography-history-release-index.csv",
    "nbs-fai-sa-history-release-index.csv",
    "nbs-fai-sa-history-selected.csv.gz",
    "nbs-households-legacy-observations.csv.gz",
    "nbs-households-legacy-release-index.csv",
    "nbs-households-legacy-source-index.csv",
    "nbs-industry-sa-history-release-index.csv",
    "nbs-industry-sa-history-selected.csv.gz",
    "nbs-migrant-history-observations.csv.gz",
    "nbs-migrant-history-release-index.csv",
    "nbs-retail-sa-history-release-index.csv",
    "nbs-retail-sa-history-selected.csv.gz",
    "nbs-tenday-history-observations.csv.gz",
    "nbs-tenday-history-release-index.csv",
    "nbs-tenday-history-targets.csv",
    "nbs-wages-history-observations.csv.gz",
    "nbs-wages-history-release-index.csv",
    "nbs-yearbook-dataset-dimensions.csv",
    "nbs-yearbook-dataset-manifest.csv",
    "nbs-yearbook-label-map.csv",
    "nbs-yearbook-observations.csv.gz",
    "nbs-yearbook-release-index.csv",
    "nbs-yearbook-targets.csv"
  )
  expect_true(all(file.exists(vapply(normalized, extdata_file, character(1L)))))

  index <- utils::read.csv(
    extdata_file("nbs-fai-sa-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "FAI seasonal-adjustment captures",
    file.path(dirname(extdata_file("nbs-fai-sa-history-release-index.csv")),
      "nbs-fai-sa-history", paste0(index$snapshot_id, ".html")),
    index$source_capture_sha256
  )

  index <- utils::read.csv(
    extdata_file("nbs-capacity-history-source-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "capacity-history captures",
    file.path(dirname(extdata_file("nbs-capacity-history-source-index.csv")),
      "nbs-capacity-history", index$retained_file),
    index$source_capture_sha256
  )

  index <- utils::read.csv(
    extdata_file("nbs-migrant-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "migrant-history captures",
    file.path(dirname(extdata_file("nbs-migrant-history-release-index.csv")),
      "nbs-migrant-history", paste0(index$period, ".html")),
    index$source_capture_sha256
  )

  for (family in c("industry", "retail")) {
    index <- utils::read.csv(
      extdata_file(paste0("nbs-", family, "-sa-history-release-index.csv")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    expect_capture(
      paste(family, "SA-history captures"),
      file.path(dirname(extdata_file(
        paste0("nbs-", family, "-sa-history-release-index.csv")
      )), paste0("nbs-", family, "-sa-history"),
      paste0(index$snapshot_id, ".html")),
      index$source_capture_sha256
    )
  }

  index <- utils::read.csv(
    extdata_file("nbs-demography-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "early-demography captures",
    file.path(dirname(extdata_file("nbs-demography-history-release-index.csv")),
      "nbs-demography-history", paste0(index$period, ".html")),
    index$source_capture_sha256
  )

  index <- utils::read.csv(
    extdata_file("nbs-tenday-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "ten-day history captures",
    file.path(dirname(extdata_file("nbs-tenday-history-release-index.csv")),
      "nbs-tenday-history", paste0(gsub("-", "", index$period), ".html")),
    index$source_capture_sha256
  )

  index <- utils::read.csv(
    extdata_file("nbs-households-legacy-source-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  household_key <- paste(index$release_period, index$residence, sep = "-")
  household_files <- c(
    "2011-urban" = "nbs-households-legacy/2012/J1005C.HTM",
    "2011-rural" = "nbs-households-legacy/2012/J1018C.HTM",
    "2012-urban" = "nbs-households-legacy/2013/Z1104C.xls",
    "2012-rural" = "nbs-households-legacy/2013/Z1117C.xls"
  )
  expect_capture(
    "legacy-household captures",
    file.path(dirname(extdata_file("nbs-households-legacy-source-index.csv")),
      household_files),
    index$source_capture_sha256[match(names(household_files), household_key)]
  )

  index <- utils::read.csv(
    extdata_file("nbs-wages-history-release-index.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_capture(
    "wage-history captures",
    file.path(dirname(extdata_file("nbs-wages-history-release-index.csv")),
      "nbs-wages-history", paste0(
        index$period, "-", ifelse(
          index$dataset_id == "NBS/CN/A/WAGES/PRIVATE_HISTORY",
          "private", "non_private"
        ), ".html"
      )),
    index$source_capture_sha256
  )

  snapshot_index <- extdata_file("nbs-yearbook-snapshot-index.csv")
  snapshot_dir <- file.path(dirname(snapshot_index), "nbs-yearbook")
  if (file.exists(snapshot_index)) {
    index <- utils::read.csv(
      snapshot_index, stringsAsFactors = FALSE, check.names = FALSE
    )
    expect_capture(
      "Yearbook snapshots",
      file.path(dirname(snapshot_index), index$asset_path),
      index$raw_sha256
    )
  } else {
    # The normalized Yearbook archive does not need the snapshot index or its
    # captures when installed; both are maintainer-only reconstruction inputs.
    expect_false(dir.exists(snapshot_dir))
  }
})
