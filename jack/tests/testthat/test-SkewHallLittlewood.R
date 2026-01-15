test_that("Skew Hall-Littlewood at t=0 is skew Schur", {
  n <- 3; lambda <- c(3, 2, 1); mu <- c(1, 1)
  skewHLpoly <- SkewHallLittlewoodPol(n, lambda, mu)
  skewSchurPoly <- SkewSchurPol(n, lambda, mu)
  expect_true(substituteParameters(skewHLpoly, 0) == skewSchurPoly)
})

test_that("Skew Hall-Littlewood for mu=[] is Hall-Littlewood", {
  n <- 3; lambda <- c(3, 2, 1); mu <- integer(0)
  skewHLpoly <- SkewHallLittlewoodPol(n, lambda, mu, "Q")
  HLpoly <- HallLittlewoodPol(n, lambda, "Q")
  expect_true(skewHLpoly == HLpoly)
})

test_that("Branching rule P", {
  lambda <- c(3, 1)
  mus <- list(integer(0), 1, 2, 3, c(1, 1), c(2, 1), c(3, 1))
  nx <- 2L
  nz <- 2L
  HLlambda <- HallLittlewoodPol(nx+nz, lambda)
  z <- list(Qlone(3), Qlone(4))
  terms <- lapply(mus, function(mu) {
    SkewHallLittlewoodPol(nx, lambda, mu) *
      changeVariables(HallLittlewoodPol(nz, mu), z)
  })
  expect_true(HLlambda == Reduce(`+`, terms))
})

test_that("Branching rule Q", {
  lambda <- c(2, 2)
  mus <- list(integer(0), 1, 2, c(1, 1), c(2, 1), c(2, 2))
  nx <- 2L
  nz <- 2L
  HLlambda <- HallLittlewoodPol(nx+nz, lambda, "Q")
  z <- list(Qlone(3), Qlone(4))
  terms <- lapply(mus, function(mu) {
    SkewHallLittlewoodPol(nx, lambda, mu, "Q") *
      changeVariables(HallLittlewoodPol(nz, mu, "Q"), z)
  })
  expect_true(HLlambda == Reduce(`+`, terms))
})
