# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# MERSKorea2015_list


library(testthat)

# Test 1: Confirm the object is a list with correct length
test_that("MERSKorea2015_list is a list of length 2", {
  expect_type(MERSKorea2015_list, "list")
  expect_equal(length(MERSKorea2015_list), 2)
})

# Test 2: Confirm the list has correct names in correct order
test_that("MERSKorea2015_list has correct element names", {
  expect_named(MERSKorea2015_list, c("linelist", "contacts"))
})

# ----------------------------
# Tests for 'linelist' element
# ----------------------------

# Test 3: Confirm linelist is a data.frame
test_that("'linelist' is a data.frame", {
  expect_s3_class(MERSKorea2015_list$linelist, "data.frame")
})

# Test 4: Confirm linelist has 162 rows and 15 columns
test_that("'linelist' has correct dimensions", {
  expect_equal(nrow(MERSKorea2015_list$linelist), 162)
  expect_equal(length(MERSKorea2015_list$linelist), 15)
})

# Test 5: Confirm linelist column names and order
test_that("'linelist' has correct column names", {
  expect_named(MERSKorea2015_list$linelist, c(
    "id", "age", "age_class", "sex", "place_infect",
    "reporting_ctry", "loc_hosp", "dt_onset", "dt_report",
    "week_report", "dt_start_exp", "dt_end_exp", "dt_diag",
    "outcome", "dt_death"
  ))
})

# Test 6: Confirm linelist column types
test_that("'linelist' columns have correct types", {
  expect_type(MERSKorea2015_list$linelist$id, "character")
  expect_type(MERSKorea2015_list$linelist$age, "integer")
  expect_type(MERSKorea2015_list$linelist$age_class, "character")
  expect_type(MERSKorea2015_list$linelist$sex, "integer")            # factor
  expect_type(MERSKorea2015_list$linelist$place_infect, "integer")   # factor
  expect_type(MERSKorea2015_list$linelist$reporting_ctry, "integer") # factor
  expect_type(MERSKorea2015_list$linelist$loc_hosp, "integer")       # factor
  expect_s3_class(MERSKorea2015_list$linelist$dt_onset, "Date")
  expect_s3_class(MERSKorea2015_list$linelist$dt_report, "Date")
  expect_type(MERSKorea2015_list$linelist$week_report, "integer")    # factor
  expect_s3_class(MERSKorea2015_list$linelist$dt_start_exp, "Date")
  expect_s3_class(MERSKorea2015_list$linelist$dt_end_exp, "Date")
  expect_s3_class(MERSKorea2015_list$linelist$dt_diag, "Date")
  expect_type(MERSKorea2015_list$linelist$outcome, "integer")        # factor
  expect_s3_class(MERSKorea2015_list$linelist$dt_death, "Date")
})

# ----------------------------
# Tests for 'contacts' element
# ----------------------------

# Test 7: Confirm contacts is a data.frame
test_that("'contacts' is a data.frame", {
  expect_s3_class(MERSKorea2015_list$contacts, "data.frame")
})

# Test 8: Confirm contacts has 98 rows and 4 columns
test_that("'contacts' has correct dimensions", {
  expect_equal(nrow(MERSKorea2015_list$contacts), 98)
  expect_equal(length(MERSKorea2015_list$contacts), 4)
})

# Test 9: Confirm contacts column names and order
test_that("'contacts' has correct column names", {
  expect_named(MERSKorea2015_list$contacts, c(
    "from", "to", "exposure", "diff_dt_onset"
  ))
})

# Test 10: Confirm contacts column types
test_that("'contacts' columns have correct types", {
  expect_type(MERSKorea2015_list$contacts$from, "character")
  expect_type(MERSKorea2015_list$contacts$to, "character")
  expect_type(MERSKorea2015_list$contacts$exposure, "integer")  # factor
  expect_type(MERSKorea2015_list$contacts$diff_dt_onset, "integer")
})
