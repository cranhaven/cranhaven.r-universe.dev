test_that("release URLs are built consistently", {
  expect_equal(
    tol_release_url("sequence_manifest.txt", base_url = "https://example.org/pub"),
    "https://example.org/pub/current_release/sequence_manifest.txt"
  )

  expect_equal(
    tol_release_url(release = "current", base_url = "https://example.org/pub"),
    "https://example.org/pub/current_release/"
  )
})

test_that("base url points to the treeoflife repository", {
  expect_equal(tol_base_url(), "https://sftp.kew.org/pub/treeoflife")
})

test_that("known bundles expose expected manifest files", {
  bundles <- tol_known_bundles()
  expect_true(all(c("bundle", "path") %in% names(bundles)))
  expect_true("sequence_manifest.txt" %in% bundles$path)
  expect_true("tree/species/treeoflife.current.tree" %in% bundles$path)
  expect_equal(tol_known_bundle(), bundles)
})

test_that("manifest column names are assigned", {
  file <- tempfile(fileext = ".txt")
  writeLines("INSDC\tERR1\tread\tSpecies alba\tPAFTOL", file)

  manifest <- tol_manifest(file, manifest = "sequence_manifest")

  expect_equal(
    names(manifest),
    c(
      "repository_name", "sequence_identifier", "sequence_type",
      "scientific_species_name", "project_name"
    )
  )
})
