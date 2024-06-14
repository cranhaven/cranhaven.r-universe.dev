context("Whale Optimization Algorithm")

test_that("Name is set", {
  expect_equal(attr(whaleOptimization(),'name'),"Whale Optimization Algorithm");
  expect_equal(attr(whaleOptimization(),'shortName'),"woa");
})