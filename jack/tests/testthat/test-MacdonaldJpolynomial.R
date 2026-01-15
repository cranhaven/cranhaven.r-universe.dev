test_that("Macdonald J-polynomial", {
  n <- 3
  macJpoly <- MacdonaldPol(n, c(2, 1), "J")
  q <- qlone(1)
  t <- qlone(2)
  mus <- list(3, c(2, 1), c(1, 1, 1))
  qtKFpolys <- list(t, 1 + q*t, q)
  tSchurPolys <- lapply(1:3, function(i) {
    qtKFpolys[[i]] * changeParameters(tSchurPol(n, mus[[i]]), list(t))
  })
  expected <- Reduce(`+`, tSchurPolys)
  expect_true(macJpoly == expected)
})

test_that("Skew Macdonald J-polynomial at q=0", {
  n <- 3
  lambda <- c(3, 2)
  mu <- c(2, 1)
  skewHLpoly <- SkewHallLittlewoodPol(n, lambda, mu, "Q")
  skewMacPolyJ <- SkewMacdonaldPol(n, lambda, mu, "J")
  q <- qlone(1)
  t <- qlone(2)
  expected <-
    changeParameters(
      changeParameters(
        skewMacPolyJ, list(qzero(), t)
      ),
      list(t, q)
    )
  expect_true(skewHLpoly == expected)
})

test_that("Macdonald J-polynomials branching rule", {
  nx <- 2
  ny <- 2
  lambda <- c(2, 2)
  macJpoly <- MacdonaldPol(nx + ny, lambda, "J")
  lones <- list(Qlone(3), Qlone(4))
  mus <- list(integer(0), 1, 2, c(1, 1), c(2, 1), c(2, 2))
  expected <-
    Reduce(
      `+`,
      lapply(mus, function(mu) {
        SkewMacdonaldPol(nx, lambda, mu, "J") *
          changeVariables(MacdonaldPol(ny, mu, "J"), lones)
      })
    )
  expect_true(macJpoly == expected)
})
