context("checkModelMCMCInputParameters")

test_that("test checkModelMCMCInputParameters produces an error if the number of samples is less than 1.", {
  
  expect_error(checkModelMCMCInputParameters(numberOfSamples = 0, burnin = 1, thin = 1), "Error: The number of samples cannot be less than 1!")
  
})

test_that("test checkModelMCMCInputParameters produces an error if the burnin is negative.", {
  
  expect_error(checkModelMCMCInputParameters(numberOfSamples = 1, burnin = -1, thin = 1), "Error: The length of burnin cannot be negative!")
  
})

test_that("test checkModelMCMCInputParameters produces an error if the thinning is negative.", {
  
  expect_error(checkModelMCMCInputParameters(numberOfSamples = 1, burnin = 1, thin = -1), "Error: The thinning parameter cannot be negative!")
  
})

test_that("test checkModelMCMCInputParameters produces an error if the number of samples is not divisible by the thinning parameter.", {
  
  expect_error(checkModelMCMCInputParameters(numberOfSamples = 3, burnin = 1, thin = 2), "Error: The number of samples requested is not divisible by the thinning parameter!")
  
})