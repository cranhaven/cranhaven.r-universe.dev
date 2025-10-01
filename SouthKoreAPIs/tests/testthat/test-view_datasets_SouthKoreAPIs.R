# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# view_datasets_SouthKoreAPIs

library(testthat)
library(SouthKoreAPIs)

test_that("view_datasets_SouthKoreAPIs works when package is loaded", {
  result <- view_datasets_SouthKoreAPIs()
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("view_datasets_SouthKoreAPIs prints correct message", {
  output <- capture_messages(view_datasets_SouthKoreAPIs())
  expect_match(
    output[1],
    "Datasets available in the 'SouthKoreAPIs' package:",
    fixed = TRUE
  )
})

test_that("view_datasets_SouthKoreAPIs returns expected datasets", {
  datasets <- view_datasets_SouthKoreAPIs()
  expected_datasets <- c(
    "AutoOwnershipKorea_df",
    "GasSales_Korea_tbl_df",
    "HeptathlonSeoul1988_df",
    "KOSPI200_list",
    "KPopIdols_tbl_df",
    "KoreanBoneDensity_df",
    "KoreanElection2017_df",
    "KoreanSocialSurvey_tbl_df",
    "MERSKorea2015_list",
    "NFIColumnNames_df",
    "RegionalKorea_df",
    "SeoulAdminAreas_sf",
    "SeoulDistrictPop_df",
    "SeoulH3Data_tbl_df",
    "SeoulMosquito_tbl_df",
    "SolarRadiation_df",
    "SouthKoreaBirths_tbl_df",
    "SouthKoreaCovid19_tbl_df",
    "demographicsKR_tbl_df",
    "migrationflows_tbl_df"




  )
  # Check if all expected datasets are present
  missing_datasets <- setdiff(expected_datasets, datasets)
  expect_true(
    length(missing_datasets) == 0,
    info = paste("Missing datasets:", paste(missing_datasets, collapse = ", "))
  )
})
