test_that("symbolicJackCombination", {
  n <- 4L
  qspray <- 3*ESFpoly(n, c(3, 1)) - 5*PSFpoly(n, c(2, 2))
  which <- "P"
  combo <- symbolicJackCombination(qspray, which)
  Qspray <- JackSymbolicCombinationToQspray(combo, n, which)
  alpha <- "2"
  expect_true(qspray == substituteParameters(Qspray, values = alpha))
})

test_that("symbolicJackCombination for a combination of Jack polynomials", {
  n <- 4L
  which <- "Q"
  alpha <- "2"
  qspray <- 3*JackPol(n, c(3, 1), alpha, which) -
    5*JackPol(n, c(2, 2), alpha, which)
  combo <- symbolicJackCombination(qspray, which)
  #
  Qspray <- JackSymbolicCombinationToQspray(combo, n, which)
  expect_true(qspray == substituteParameters(Qspray, values = alpha))
  #
  combo <- lapply(combo, function(t) {
    list(
      "coeff" = evalRatioOfQsprays(t[["coeff"]], alpha),
      "lambda" = t[["lambda"]]
    )
  })
  combo <- Filter(function(t) {
    t[["coeff"]] != 0L
  }, combo)
  #
  lambdas <- names(combo)
  expect_identical(lambdas, c("[3, 1]", "[2, 2]"))
  #
  coeffs <- lapply(combo, `[[`, "coeff")
  expect_true(coeffs[[1L]] == 3)
  expect_true(coeffs[[2L]] == -5)
})

test_that("symbolicJackCombination for a non-homogeneous polynomial", {
  n <- 4L
  qspray <- ESFpoly(n, c(3, 1)) + PSFpoly(n, c(2, 1))
  which <- "J"
  combo <- symbolicJackCombination(qspray, which)
  Qspray <- JackSymbolicCombinationToQspray(combo, n, which)
  alpha <- "3"
  expect_true(qspray == substituteParameters(Qspray, values = alpha))
})

test_that("symbolicJackCombination for a 'degenerate' symmetric polynomial", {
  n <- 3L
  qspray <- ESFpoly(n, c(3, 1)) + PSFpoly(n, c(2, 2))
  which <- "C"
  combo <- symbolicJackCombination(qspray, which)
  Qspray <- JackSymbolicCombinationToQspray(combo, n, which)
  alpha <- "3/2"
  expect_true(qspray == substituteParameters(Qspray, values = alpha))
})

test_that("symbolicJackCombination for a symbolicQspray", {
  n <- 4L
  which <- "Q"
  x <- qlone(1L)
  Qspray <- 3*JackSymPol(n, c(3, 1), which) - 5*x*JackSymPol(n, c(2, 2), which)
  combo <- symbolicJackCombination(Qspray, which)
  #
  lambdas <- names(combo)
  expect_identical(lambdas, c("[3, 1]", "[2, 2]"))
  #
  coeffs <- lapply(combo, `[[`, "coeff")
  expect_true(coeffs[[1L]] == 3)
  expect_true(coeffs[[2L]] == -5*x)
})
