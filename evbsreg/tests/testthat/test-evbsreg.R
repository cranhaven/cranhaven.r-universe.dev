test_that("evbsreg.fit reproduces the published Itajai estimates", {
  data(itajai)
  X <- cbind(1, itajai$pressure)
  fit <- evbsreg.fit(X, itajai$wind)

  # Published estimates (paper Table 2)
  expect_equal(unname(fit$coeff[1]), 25.5148, tolerance = 1e-3)
  expect_equal(unname(fit$coeff[2]), -0.0227, tolerance = 1e-3)
  expect_equal(unname(fit$alphahat),  0.1857, tolerance = 1e-3)
  expect_equal(unname(fit$gamahat),  -0.1551, tolerance = 1e-3)
})

test_that("cnc_diagnostics flags observation 82", {
  data(itajai)
  X <- cbind(1, itajai$pressure)
  fit <- evbsreg.fit(X, itajai$wind)
  diag <- cnc_diagnostics(fit)

  # Observation 82 has the largest aggregate contribution at q=7
  expect_equal(which.max(diag$Bj[7, ]), 82L)

  # Top normalized eigenvalue matches the paper
  expect_equal(diag$eigenvalues_norm[1], 0.69678, tolerance = 1e-4)
})

test_that("deletion of obs 82 changes gamma by about -73.67%", {
  data(itajai)
  X <- cbind(1, itajai$pressure)
  fit <- evbsreg.fit(X, itajai$wind)
  fit82 <- evbsreg.fit(X[-82, ], itajai$wind[-82])
  rc <- 100 * (fit82$gamahat - fit$gamahat) / abs(fit$gamahat)
  expect_equal(unname(rc), -73.67, tolerance = 0.1)
})

test_that("revbs returns the requested number of positive variates", {
  set.seed(2023)
  x <- revbs(50, alpha = 0.5, beta = 1, gama = 0.2)
  expect_length(x, 50)
  expect_true(all(x > 0))
})
