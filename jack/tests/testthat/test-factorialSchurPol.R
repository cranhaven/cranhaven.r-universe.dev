test_that("Factorial Schur polynomial with a=(0, ...) is Schur polynomial", {
  n <- 4
  lambda <- c(3, 3, 2, 2)
  a <- rep(0, n + lambda[1] - 1)
  factorialSchurPoly <- factorialSchurPol(n, lambda, a)
  schurPoly <- SchurPol(n, lambda)
  expect_true(factorialSchurPoly == schurPoly)
})

test_that("factorialSchurPol: comparison with Haskell", {
  factorialSchurPoly <- factorialSchurPol(3, c(2, 1, 1), c(2, 6, 1, 2))
  x <- qlone(1); y <- qlone(2); z <- qlone(3)
  expected <- x^2*y*z + 2*x^2*y + 2*x^2*z + 4*x^2 + x*y^2*z + 2*x*y^2 +
    x*y*z^2 + 15*x*y*z + 26*x*y + 2*x*z^2 + 26*x*z + 44*x + 2*y^2*z + 4*y^2 +
    2*y*z^2 + 26*y*z + 44*y + 4*z^2 + 44*z + 72
  expect_true(factorialSchurPoly == expected)
})
