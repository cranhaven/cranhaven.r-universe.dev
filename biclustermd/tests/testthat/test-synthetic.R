context("Synthetic class")

test_that("Synthetic is a matrix", {
  expect_equal(inherits(synthetic, "matrix"), TRUE)
})
