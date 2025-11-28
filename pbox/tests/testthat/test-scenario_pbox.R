library(testthat)
library(data.table)
library(copula)


# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  pbx<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam)])
  param_list <- list(Malaysia = c("mu","sigma"))
  result <- scenario_pbox(pbx, param_list, sigma = 0.05, range = seq(-3, 3, 1),mj="Malaysia:33 & Thailand:33")
  # Check if the result is a list
  expect_type(result, "list")

  # Check if the list has the correct names
  expected_names <- paste0("SD", seq(-3, 3, 1))
  expect_named(result, expected_names)

  # Check if each element in the list is a numeric value
  for (res in result) {
    expect_type(res, "double")
  }
})

# Test handling of invalid pbox input
test_that("Test handling of invalid pbox input", {
  invalid_pbx <- "not_a_pbox"
  param_list <- list(Malaysia = "mean")
  expect_error(scenario_pbox(invalid_pbx, param_list, sigma = 0.05, range = seq(-3, 3, 1)))
})

# Test handling of empty param_list input
test_that("Test handling of empty param_list input", {
  pbx<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam)])
  param_list <- list()
  expect_error(scenario_pbox(pbx, param_list, sigma = 0.05, range = seq(-3, 3, 1),mj="Malaysia:33 & Thailand:33"))

})

# Test handling of different sigma values
test_that("Test handling of different sigma values", {
  pbx<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam)])
  param_list <- list(Malaysia = "mu")
  result <- scenario_pbox(pbx, param_list, sigma = 0.1, range = seq(-3, 3, 1),mj="Malaysia:33 & Thailand:33")

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the list has the correct names
  expected_names <- paste0("SD", seq(-3, 3, 1))
  expect_named(result, expected_names)

  # Check if each element in the list is a numeric value
  for (res in result) {
    expect_type(res, "double")
  }
})

# Test handling of different range values
test_that("Test handling of different range values", {
  pbx<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam)])
  param_list <- list(Malaysia = "mu")
  result <- scenario_pbox(pbx, param_list, sigma = 0.05, range = seq(-5, 5, 2),mj="Malaysia:33 & Thailand:33")

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the list has the correct names
  expected_names <- paste0("SD", seq(-5, 5, 2))
  expect_named(result, expected_names)

  # Check if each element in the list is a numeric value
  for (res in result) {
    expect_type(res, "double")
  }
})

# Test handling of non-numeric range input
test_that("Test handling of non-numeric range input", {
  pbx<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam)])
  param_list <- list(Malaysia = "mu")
  expect_error(scenario_pbox(pbx, param_list, sigma = 0.05, range = "not_a_vector",mj="Malaysia:33 & Thailand:33"))
})

