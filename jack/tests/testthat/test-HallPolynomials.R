test_that("A Hall polynomial (comparison with Sage)", {
  hallPolys <- HallPolynomials(c(3, 1, 1), c(2, 1))
  lambda <- c(4, 2, 1, 1)
  hallPoly <- hallPolys[[partitionAsString(lambda)]][["polynomial"]]
  t <- qlone(1)
  expected <- 2*t^3 + t^2 - t - 1
  expect_true(hallPoly == expected)
})

test_that("Leading coefficient of Hall polynomial is Littlewood-Richardson", {
  mu <- c(3, 1)
  nu <- c(2, 1)
  lrCoeffs <- LRmult(mu, nu, output = "dataframe")
  coeffs <- lrCoeffs[, "coeff"]
  lambdasAsStrings <- lrCoeffs[, "lambda"]
  names(coeffs) <- lambdasAsStrings
  hallPolys <- HallPolynomials(mu, nu)
  lambdasAsStrings <- names(hallPolys)
  leadingCoeffs <- gmp::c_bigq(lapply(hallPolys, function(x) {
    leadingCoefficient(x[["polynomial"]])
  }))
  expect_true(all(coeffs[lambdasAsStrings] == leadingCoeffs))
})
