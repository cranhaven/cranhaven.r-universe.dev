# Asiaverse - A Metapackage for Asian Countries RESTful APIs and Curated Datasets
# Version 0.1.0
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# asiaverse

library(testthat)
library(Asiaverse)

test_that("Asiaverse function returns correct package names", {
  expected_pkgs <- c(
    "ChinAPIs",
    "JapanAPIs",
    "SouthKoreAPIs",
    "IndiAPIs",
    "IndonesiAPIs"
  )

  result <- Asiaverse()
  expect_type(result, "character")
  expect_true(all(expected_pkgs %in% result))
})

test_that("All listed packages are installed", {
  pkgs <- Asiaverse()
  for (pkg in pkgs) {
    expect_true(requireNamespace(pkg, quietly = TRUE),
                info = paste("Package", pkg, "is not installed."))
  }
})

test_that("Function does not throw errors", {
  expect_error(Asiaverse(), NA)
})
