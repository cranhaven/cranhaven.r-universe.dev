test_that("permute/swap", {
  x <- qlone(1); y <- qlone(2); z <- qlone(3)
  p <- x^4 + 2*y^3 + 3*z^2 - 4
  expect_true(permuteVariables(p, c(1, 3, 2)) == swapVariables(p, 2, 3))
  expect_error(permuteVariables(p, c(1, 3, 4)))
})