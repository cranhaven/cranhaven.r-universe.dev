test_that("Skew factorial Schur polynomial with a=(0, ...) is skew Schur polynomial", {
  n <- 4
  lambda <- c(3, 3, 2, 2)
  mu <- c(2, 2)
  a <- rep(0, 3 + n + lambda[1] - 1)
  skewFactorialSchurPoly <- SkewFactorialSchurPol(n, lambda, mu, a, i0 = 3)
  skewSchurPoly <- SkewSchurPol(n, lambda, mu)
  expect_true(skewFactorialSchurPoly == skewSchurPoly)
})

test_that("SkewFactorialSchurPol: comparison with Haskell", {
  skewFactorialSchurPoly <-
    SkewFactorialSchurPol(3, c(3, 2, 2), c(2, 1), c(2, 6, 1, 2, 3, 4, 5, 6), 3)
  x <- qlone(1); y <- qlone(2); z <- qlone(3)
  expected <- x^3*y + x^3*z + 5*x^3 + 2*x^2*y^2 + 4*x^2*y*z + 37*x^2*y +
    2*x^2*z^2 + 37*x^2*z + 135*x^2 + x*y^3 + 4*x*y^2*z + 37*x*y^2 +
    4*x*y*z^2 + 81*x*y*z + 375*x*y + x*z^3 + 37*x*z^2 + 375*x*z + 1063*x +
    y^3*z + 5*y^3 + 2*y^2*z^2 + 37*y^2*z + 135*y^2 + y*z^3 + 37*y*z^2 +
    375*y*z + 1063*y + 5*z^3 + 135*z^2 + 1063*z + 2445
  expect_true(skewFactorialSchurPoly == expected)
})
