test_that("JackSymPol J", {
  n <- 3
  lambda <- c(3, 1, 1)
  alpha <- gmp::as.bigq("2/3")
  symbolicJackPolynomial <- JackSymPol(n, lambda)
  JackPolynomial <- JackPolR(n, lambda, alpha)
  x <- substituteParameters(symbolicJackPolynomial, alpha)
  expect_true(x == JackPolynomial)
  #
  n <- 4
  JackPolynomial <- JackSymPol(n, c(3, 1))
  a <- qlone(1)
  M31 <- as(MSFpoly(n, c(3, 1)), "symbolicQspray")
  M211 <- as(MSFpoly(n, c(2, 1, 1)), "symbolicQspray")
  M22 <- as(MSFpoly(n, c(2, 2)), "symbolicQspray")
  M1111 <- as(MSFpoly(n, c(1, 1, 1, 1)), "symbolicQspray")
  expected <- (2*a^2 + 4*a + 2)*M31 + (6*a + 10)*M211 + (4*a + 4)*M22 +
    24*M1111
  expect_true(JackPolynomial == expected)
})

test_that("JackSymPol P", {
  n <- 3
  lambda <- c(3, 2, 1)
  symbolicJackPolynomial <- JackSymPol(n, lambda, which = "P")
  #
  alpha <- gmp::as.bigq("2/3")
  JackPolynomial <- JackPolR(n, lambda, alpha, which = "P")
  x <- substituteParameters(symbolicJackPolynomial, alpha)
  expect_true(x == JackPolynomial)
  #
  SchurPolynomial <- SchurPolR(n, lambda)
  x <- substituteParameters(symbolicJackPolynomial, "1")
  expect_true(x == SchurPolynomial)
})

test_that("JackSymPol Q", {
  n <- 3
  lambda <- c(4, 2, 2)
  alpha <- gmp::as.bigq("2/3")
  symbolicJackPolynomial <- JackSymPol(n, lambda, which = "Q")
  JackPolynomial <- JackPolR(n, lambda, alpha, which = "Q")
  x <- substituteParameters(symbolicJackPolynomial, alpha)
  expect_true(x == JackPolynomial)
})

test_that("JackSymPol C", {
  n <- 3
  lambda <- c(3, 2, 2)
  symbolicJackPolynomial <- JackSymPol(n, lambda, which = "C")
  #
  ZonalPolynomial <- ZonalPolR(n, lambda)
  x <- substituteParameters(symbolicJackPolynomial, "2")
  expect_true(x == ZonalPolynomial)
  #
  ZonalQPolynomial <- ZonalQPolR(n, lambda)
  x <- substituteParameters(symbolicJackPolynomial, "1/2")
  expect_true(x == ZonalQPolynomial)
})

test_that("JackSymPol is symmetric", {
  n <- 3
  lambda <- c(4, 2, 2)
  symbolicJackPolynomial <- JackSymPol(n, lambda)
  expect_true(isSymmetricQspray(symbolicJackPolynomial))
})

test_that("JackSymPol J has polynomial coefficients only", {
  n <- 5
  lambda <- c(4, 4, 3, 2, 1)
  J <- JackSymPol(n, lambda)
  expect_true(hasPolynomialCoefficientsOnly(J))
})
