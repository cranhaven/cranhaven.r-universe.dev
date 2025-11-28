test_that("limits works", {
  expect_equal(limits("greater", 0.9), c(0.1, 1.0))
  expect_equal(limits("less", 0.9), c(0, 0.9))
  expect_equal(limits("two.sided", 0.9), c(0.05, 0.95))
})
