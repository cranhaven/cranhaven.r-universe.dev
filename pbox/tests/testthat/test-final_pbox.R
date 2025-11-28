# test-final_pbox.R
library(testthat)
library(data.table)
library(copula)

data("SEAex")
copulaFits <- fit_copula_pbox(data = SEAex, .copula_families)
distFits <- fit_dist_pbox(data = SEAex)


# Test for successful creation of the final_pbox output
test_that("final_pbox correctly creates mvdc object with the best copula and marginal distributions", {
  # Mock dataset to be used as input
  # Call the final_pbox function with the mocked input
  final_result <- final_pbox(copulaFits, distFits$allDitrs, SEAex)

  # Check the structure and properties of the returned mvdc object
  expect_s4_class(final_result, "mvdc")

  # Ensure the copula and marginals were set correctly
  expect_equal(final_result@copula@dispstr, "ex")
  expect_equal(final_result@margins, c("RG" ,  "SN1" ,  "RG" ,"RG"))

  # Check that the correct copula parameters were applied
  expect_equal(round(final_result@copula@parameters,2), 0.49)

})
