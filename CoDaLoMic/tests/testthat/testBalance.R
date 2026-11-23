#context("Balance")


test_that("Balance", {
  expect_equal(Balance(2,2,cbind(c(0.1,0.1,0.8),c(0.2,0.1,0.7)),3,3), c(-2.079442,-1.252763),tolerance=1e-6)
  })
