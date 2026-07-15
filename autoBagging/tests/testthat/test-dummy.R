context("dummy")

test_that("mode in value equal mode x", {
  x <- rnorm(4L)
  y <- 5.
  expect_equal(mode(x), mode(y))
})