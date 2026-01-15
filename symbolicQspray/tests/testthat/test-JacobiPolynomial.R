test_that("Jacobi polynomial for n=2", {
  obtained <- JacobiPolynomial(2)
  X     <- Qlone(1)
  alpha <- qlone(1)
  beta  <- qlone(2)
  expected <- (alpha + 1L)*(alpha + 2L)/2L +
    (alpha + 2L)*(alpha + beta + 3L) * (X - 1L)/2L +
    (alpha + beta + 3L)*(alpha + beta + 4L)/2L * ((X - 1L)/2L)^2L
  expect_true(obtained == expected)
})

test_that("Jacobi polynomial coefficients are polynomial", {
  JP <- JacobiPolynomial(7)
  expect_true(hasPolynomialCoefficientsOnly(JP))
})

test_that("Jacobi polynomial differential equation", {
  n <- 5L
  JP <- JacobiPolynomial(n)
  JPprime      <- derivSymbolicQspray(JP, 1)
  JPprimeprime <- derivSymbolicQspray(JP, 1, 2)
  X     <- Qlone(1)
  alpha <- qlone(1)
  beta  <- qlone(2)
  shouldBeZero <- (1L - X^2)*JPprimeprime +
    (beta - alpha - (alpha + beta + 2L)*X)*JPprime -
    n*(n + alpha + beta + 1L)*JP
  expect_true(shouldBeZero == 0L)
})
