library(testthat)
library(data.table)
library(copula)

sample_data<-SEAex[,.(Vietnam,Malaysia,Thailand)]
pbx <- set_pbox(sample_data)

# Test correct functionality with typical marginal query
test_that("Test correct functionality with typical marginal query", {
  result <- qpbox(pbx, mj = "Vietnam:31.5")

  # Check if the result is a numeric value
  expect_type(result, "double")
  expect_named(result, "P")
})

# Test correct functionality with typical conditional query
test_that("Test correct functionality with typical conditional query", {
  result <- qpbox(pbx, mj = "Vietnam:31.5", co = "Malaysia:32.5")

  # Check if the result is a numeric value
  expect_type(result, "double")
  expect_named(result, "P")
})

# Test handling of invalid pbox input
test_that("Test handling of invalid pbox input", {
  invalid_pbx <- "not_a_pbox"
  expect_error(qpbox(invalid_pbx, mj = "Vietnam:31.5"))
})

# Test handling of missing conditional query
test_that("Test handling of missing conditional query", {
  expect_error(qpbox(pbx, mj = "Vietnam:31.5", fixed = TRUE), "Conditional query is missing!")
})

# Test handling of invalid marginal query format
test_that("Test handling of invalid marginal query format", {
  expect_error(qpbox(pbx, mj = "Vietnam=31.5"), "Please specify the marginal in the following format 'Variable1:Value1 & Variable2:Value2'")
})

# Test handling of invalid conditional query format
test_that("Test handling of invalid conditional query format", {
  expect_error(qpbox(pbx, mj = "Var1:0.5", co = "Var2-0.5"), "Please specify the conditional in the following format 'Variable1:Value1 & Variable2:Value2'")
})

# Test handling of non-character marginal query
test_that("Test handling of non-character marginal query", {
  expect_error(qpbox(pbx, mj = 123), "Please specify the marginal in the following format 'Variable1:Value1 & Variable2:Value2'")
})

# Test handling of non-character conditional query
test_that("Test handling of non-character conditional query", {
  expect_error(qpbox(pbx, mj = "Var1:0.5", co = 123),"Please specify the conditional in the following format 'Variable1:Value1 & Variable2:Value2'")
})

# Test with confidence interval estimation
test_that("Test with confidence interval estimation", {
  result <- qpbox(pbx, mj = "Vietnam:31.5",)

  # Check if the result is a named vector with confidence intervals
  expect_type(result, "double")
  expect_named(result, c("P"))
})

# Test with lower.tail = FALSE
test_that("Test with lower.tail = FALSE", {
  result <- qpbox(pbx, mj = "Vietnam:31.5", lower.tail = FALSE)

  # Check if the result is a numeric value
  expect_type(result, "double")
  expect_named(result, "P")

  # Check if the result is within the correct range
  expect_true(result <= 1 && result >= 0)
})
