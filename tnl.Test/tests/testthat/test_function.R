library(testthat) # load testthat package
library(tnl.Test) # load our package

# Test whether the output is a list
test_that("functions returns a list", {
  x <- c(1.0021, -0.0128, 2.739, 1.605, 1.084, 3.072, 2.417)
  y <- c(-1.159, -0.038, -0.374, 0.900, -1.1063, 2.739, 3.104)
  expect_type(tnl(4, 5, 1), "list")
  expect_type(tnl.sim(3, 3, 1), "list")
  expect_type(ptnl(2, 4, 4, 1), "list")
  expect_type(ptnl(c(2, -1, 6), 5, 5, 1, exact = "TRUE"), "list")
  expect_type(ptnl(2, 4, 4, 1, exact = "FALSE"), "list")
  expect_type(ptnl(2, 11, 8, 1), "list")
  expect_type(dtnl(2, 5, 4, 1), "list")
  expect_type(dtnl(2, 5, 4, 1), "list")
  expect_type(dtnl(c(2, -1, 6), 5, 5, 1, exact = "TRUE"), "list")
  expect_type(dtnl(2, 5, 4, 1, exact = "FALSE"), "list")
  expect_type(dtnl(2, 11, 5, 1), "list")
  expect_type(qtnl(.2, 5, 7, 1), "list")
  expect_type(qtnl(.2, 5, 5, 1), "list")
  expect_type(qtnl(.2, 5, 7, 1, exact = "TRUE"), "list")
  expect_type(qtnl(.2, 5, 7, 1, exact = "FALSE"), "list")
  expect_type(qtnl(.2, 11, 9, 1), "list")
  expect_type(tnl.test(x, y, l = 2), "list")
  expect_type(tnl.test(x, y, l = 2, exact = "TRUE"), "list")
  expect_type(tnl.test(x, y, l = 1, exact = "FALSE"), "list")
  X <- c(x, 0.975, 1.144, 0.572, -0.532)
  Y <- c(y, 1.007, -2.023, 1.468, 1.396)
  expect_type(tnl.test(X, Y, l = 2), "list")
  expect_type(tnl_mean(10, 7, 2), "double")
  expect_type(tnl_mean(7, 7, 2), "double")
  expect_type(ptnl.lehmann(2, 6, 6, 2, 0.5), "double")
  expect_type(dtnl.lehmann(2, 6, 5, 2, 1.2), "double")
  expect_type(qtnl.lehmann(.351, 6, 5, 2, 0.5), "integer")
  expect_type(dtnl.lehmann(2, 6, 6, 2, 1.2), "double")
  expect_type(qtnl.lehmann(.351, 6, 6, 2, 0.5), "integer")
})
## Test whether the output return the right number
test_that("functions returns the right output", {
  expect_equal(sum(tnl(7, 4, 1)$pmf), 1)
  #expect_equal(sum(tnl.sim(4, 4, 1)$pmf), 1)
  expect_equal(tnl(5, 4, 1)$cdf[4], 1)
  #expect_equal(tnl.sim(4, 4, 1)$cdf[4], 1)
  expect_equal(ptnl(4, 5, 4, 1)$cdf, 1)
  expect_equal(ptnl.lehmann(6, 6, 6, 2, 0.5), 1)
})
# ## Test whether the output contains the right number
test_that("functions returns a list with the specified length", {
  expect_length(tnl(7, 7, 1)$pmf, 7)
  expect_length(tnl(7, 7, 1)$cdf, 7)
  #expect_length(tnl.sim(5, 5, 1)$pmf, 5)
  #expect_length(tnl.sim(5, 5, 1)$cdf, 5)
  expect_length(ptnl(2, 7, 8, 1)$cdf, 1)
  expect_length(dtnl(2, 7, 9, 1)$pmf, 1)
  # expect_length(ptnl(2, 11, 8, 1)$cdf, 1)
  # expect_length(dtnl(2, 11, 8, 1)$pmf, 1)
  expect_length(qtnl(.2, 7, 8, 1)$quantile, 1)
  expect_length(tnl_mean(10, 13, 3), 1)
  expect_length(ptnl.lehmann(4, 5, 8, 1, 1.2), 1)
  expect_length(dtnl.lehmann(2, 6, 8, 2, 0.5), 1)
  expect_length(qtnl.lehmann(.2, 6, 8, 2, 0.5), 1)
})
# ## Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {
  expect_vector(rtnl(10, 5, 10, 2), ptype = double(), size = 10)
  expect_vector(
    dtnl.lehmann(c(2, -1, 8), 6, 9, 2, 0.8), ptype = double(), size = 3)
  expect_vector(
    ptnl.lehmann(c(2, -1, 6), 5, 9, 2, 1.2), ptype = double(), size = 3)
  expect_vector(rtnl.lehmann(10, 7, 7, 2, 1), ptype = double(), size = 10)
  expect_vector(rtnl.lehmann(10, 7, 9, 2, 1), ptype = double(), size = 10)
})
# ## Test whether the output should not exceed one.
test_that("functions returns number should not exceed one", {
  x <- c(1.0021, -0.0128, 2.739, 1.605, 1.084, 3.072, 2.417)
  y <- c(-1.159, -0.038, -0.374, 0.900, -1.1063, 2.739, 3.104)
  expect_lte(tnl(7, 7, 1)$pmf[2], 1)
  expect_lte(tnl(7, 7, 1)$cdf[5], 1)
  #expect_lte(tnl.sim(5, 5, 1)$pmf[4], 1)
  #expect_lte(tnl.sim(5, 5, 1)$cdf[3], 1)
  expect_lte(ptnl(2, 7, 7, 1)$cdf, 1)
  expect_lte(dtnl(2, 7, 7, 1)$pmf, 1)
  expect_lte(tnl.test(x, y, l = 3)$p.value, 1)
})
# ## Test whether the output should exceed zero.
test_that("functions returns number should exceed zero", {
  x <- c(1.0021, -0.0128, 2.739, 1.605, 1.084, 3.072, 2.417)
  y <- c(-1.159, -0.038, -0.374, 0.900, -1.1063, 2.739, 3.104)
  expect_lte(0, tnl(7, 8, 1)$pmf[5])
  expect_lte(0, tnl(7, 7, 1)$cdf[2])
  # expect_lte(0, tnl.sim(5, 5, 1)$pmf[4])
  # expect_lte(0, tnl.sim(5, 5, 1)$cdf[3])
  expect_lte(0, ptnl(2, 7, 7, 1)$cdf)
  expect_lte(0, dtnl(2, 7, 7, 1)$pmf)
  expect_lte(0, tnl.test(x, y, l = 3)$p.value)
})

# ## Test whether the code throw an error.
test_that("functions returns errors", {
  x <- c(1.0021, -0.0128, 2.739, 1.605, 1.084, 3.072, 2.417)
  y <- c(-1.159, -0.038, -0.374, 0.900, -1.1063, 2.739, 3.104)
  expect_error(tnl(4, 8, 2), "n,m must be > 2l")
  expect_error(tnl.sim(4, 5, 2), "n,m must be > 2l")
  expect_error(ptnl(3, 4, 6, 2), "n,m must be > 2l")
  expect_error(dtnl(3, 4, 7, 2), "n,m must be > 2l")
  expect_error(qtnl(.3, 4, 10, 2), "n,m must be > 2l")
  expect_error(qtnl(1.3, 8, 15, 1), "p must be between 0 and 1")
  expect_error(dtnl.lehmann(2, 3, 9, 5, 1.2), "n,m must be > 2l")
  expect_error(ptnl.lehmann(2, 3, 9, 5, 1.2), "n,m must be > 2l")
  expect_warning(
    tnl.test(c(x, NA), c(NA, y), l = 2),
    "Since the data should not contain missing values,
we exclude the missing values from the data"
  )
  expect_warning(tnl_mean(5, 14, 3), "n,m must be > 2l")
  expect_error(rtnl(20, 5, 10, 3), "n,m must be > 2l")
  expect_error(rtnl.lehmann(20, 5, 18, 3, 1), "n,m must be > 2l")
  expect_error(qtnl.lehmann(3, 5, 9, 2, 1.2), "p must be between 0 and 1")
  expect_error(qtnl.lehmann(.3, 4, 6, 2, 0.5), "n,m must be > 2l")
})
