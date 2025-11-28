library(testthat)
library(copula) # Assuming this package provides the needed functionality for copulas
library(data.table) # For data manipulation in the function

# Sample data
data("SEAex")

# Define copula families to test
.copula_families <- list(
  archmCopula = c("clayton", "frank", "gumbel", "joe"),
  evCopula = c("galambos", "gumbel", "huslerReiss"),
  ellipCopula = c("normal")
)

# Define a helper function to simulate testing if necessary
simulate_data <- function(n, seed = 123) {
  set.seed(seed)
  data.table(matrix(runif(n * 2), ncol = 2))
}

# Test with expected data
test_that("Test with predefined data", {
  result <- fit_copula_pbox(data = SEAex, .copula_families = .copula_families)
  expect_type(result, "list")
  expect_true(ncol(result) > 0)
  expect_true(nrow(result) > 0)
  expect_true("AIC" %in% colnames(result))
})

# Test with empty data
test_that("Test with empty data", {
  empty_data <- data.table()
  expect_error(fit_copula_pbox(data = empty_data, .copula_families = .copula_families))
})

# Test with incorrect data types
test_that("Test with incorrect data types", {
  expect_error(fit_copula_pbox(data = "not_a_dataframe", .copula_families = .copula_families))
})

# Test for correct handling of unsupported copula families
test_that("Handle unsupported copula families", {
  unsupported_families <- list(archmCopula = c("nonexistent"))
  expect_error(fit_copula_pbox(data = SEAex, .copula_families = unsupported_families))
})
