# DataSetsVerse - A Metapackage for Thematic and Domain-Specific Datasets in R
# Version 0.1.0
# Copyright (C) 2025 Renzo Caceres Rossi
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(testthat)

test_that("DataSetsVerse function returns correct package names", {
  expected_pkgs <- c(
    "timeSeriesDataSets",
    "educationR",
    "crimedatasets",
    "MedDataSets",
    "OncoDataSets"
  )

  result <- DataSetsVerse()
  expect_type(result, "character")
  expect_true(all(expected_pkgs %in% result))
})

test_that("All listed packages are installed", {
  pkgs <- DataSetsVerse()
  for (pkg in pkgs) {
    expect_true(requireNamespace(pkg, quietly = TRUE),
                info = paste("Package", pkg, "is not installed."))
  }
})

test_that("Function does not throw errors", {
  expect_error(DataSetsVerse(), NA)
})
