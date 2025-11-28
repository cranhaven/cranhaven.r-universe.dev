library(testthat)
library(copula)
library(data.table)

# Define sample pseudo-observations
u <- pobs(SEAex[,.(Vietnam, Thailand)])

# Sample copula families to test
families <- c("clayton", "frank", "gumbel", "joe")

# Define a helper function to simulate testing if necessary
simulate_data <- function(dim, seed = 123) {
  set.seed(seed)
  matrix(runif(100 * dim), ncol = dim)
}

# Test with correct input
test_that("Test with correct input", {
  for (family in families) {
    result <- .fit_copula(copula = "archmCopula", family = family, dim = 2, u = u)
    expect_type(result, "list")
    expect_true(ncol(result) == 4)
    expect_true(nrow(result) == 1)
    expect_true("AIC" %in% colnames(result))
    expect_true("coef" %in% colnames(result))
  }
})

# Test handling unsupported dimensions
test_that("Handle unsupported dimensions", {
  expect_error(.fit_copula(copula = "archmCopula", family = "clayton", dim = 3, u = u))
})

# Test with incorrect data types
test_that("Test with incorrect data types", {
  expect_error(.fit_copula(copula = "not_a_copula", family = "clayton", dim = 2, u = "not_a_matrix"))
})

# Test for correct handling of unsupported copula families
test_that("Handle unsupported copula families", {
  unsupported_families <- c("nonexistent", "fake")
  for (family in unsupported_families) {
    expect_error(.fit_copula(copula = "archmCopula", family = family, dim = 2, u = u))
  }
})
