context("Test NINO stacks")

test_that("a new NINO stack is empty, is a 'stack' object, 
          and is a 'nino' object", {
  s <- nino()
  expect_true(is.empty(s))
  expect_true(is.stack(s))
  expect_true(is.nino(s))
  x <- stats::runif(1)
  push(s, x)
  expect_error(pop(s))
})
