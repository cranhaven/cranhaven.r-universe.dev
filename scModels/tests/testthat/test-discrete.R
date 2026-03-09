
test_that("Zero probabilities for non-integers", {
  expect_warning(expect_equal(dpb(1.5, 5, 3, 20), 0))
})

test_that("Zero probabilities for negative integers", {
  expect_equal(dpb(-1, 5, 3, 20), 0)
})

test_that("cdf vs cumsum(pdf)", {
  x <- seq(0, 500, by=1)
  epsilon <- 1e-4
  expect_equal(cumsum(dpb(x, 5, 3, 20)), ppb(x, 5, 3, 20), tolerance = epsilon)
})
