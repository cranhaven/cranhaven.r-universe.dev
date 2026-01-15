test_that("qt-Kostka polynomials", {
  n <- 4
  mu <- c(2, 1, 1)
  macJpoly <- MacdonaldPol(n, mu, "J")
  qtKostkaPolys <- qtKostkaPolynomials(mu)
  t <- qlone(2)
  expected <- Reduce(
    `+`,
    lapply(qtKostkaPolys, function(lambda_kp) {
      lambda <- lambda_kp[["lambda"]]
      kp <- lambda_kp[["polynomial"]]
      tSchurPoly <- tSchurPol(n, lambda)
      kp * changeParameters(tSchurPoly, list(t))
    })
  )
  expect_true(macJpoly == expected)
})

test_that("Skew qt-Kostka polynomials", {
  lambda <- c(2, 1, 1)
  mu <- c(1, 1)
  qtSkewKostkaPolys <- qtSkewKostkaPolynomials(lambda, mu)
  q <- qlone(1)
  t <- qlone(2)
  expected <- lapply(qtSkewKostkaPolys, function(nu_poly) {
    changeVariables(
      changeVariables(
        nu_poly[["polynomial"]], list(qzero(), t)
      ),
      list(t, q)
    )
  })
  skewKFpolys <- lapply(qtSkewKostkaPolys, function(nu_poly) {
    SkewKostkaFoulkesPolynomial(lambda, mu, nu_poly[["nu"]])
  })
  check <- mapply(
    `==`,
    expected, skewKFpolys,
    SIMPLIFY = TRUE
  )
  expect_true(all(check))
})
