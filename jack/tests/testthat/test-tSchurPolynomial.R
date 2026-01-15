test_that("t-Schur polynomial", {
  tSchurPoly <- tSchurPol(2, c(2, 1))
  t <- qlone(1)
  x <- Qlone(1)
  y <- Qlone(2)
  expected <-
    (1-t) * ((1 - t)^2 * (x^2 * y + x * y^2) - t * (x^3 + y^3))
  expect_true(tSchurPoly == expected)
})

test_that("Skew t-Schur polynomial - branching rule", {
  lambda <- c(2, 2)
  tSchurPoly <- tSchurPol(4, lambda)
  ys <- list(Qlone(3), Qlone(4))
  expected <-
    Reduce(
      `+`,
      lapply(list(integer(0), 1, 2, c(1, 1), c(2, 1), c(2, 2)), function(mu) {
        tSkewSchurPol(2, lambda, mu) *
          changeVariables(tSchurPol(2, mu), ys)
      })
    )
  expect_true(tSchurPoly == expected)
})
