test_that("GEV analytic score matches numerical differentiation", {
  data(itajai)
  X <- cbind(1, itajai$pressure - mean(itajai$pressure))
  y <- itajai$wind
  f <- gevreg.fit(X, y)
  S <- gev_scores(y, X, f$beta, f$sigma, f$xi)
  lli <- function(par, i) {
    mu <- sum(X[i, ] * par[1:2]); s <- par[3]; x <- par[4]
    z <- (y[i] - mu) / s; u <- 1 + x * z
    -log(s) - (1 + 1 / x) * log(u) - u^(-1 / x)
  }
  p0 <- c(f$beta, f$sigma, f$xi); h <- 1e-6
  num <- vapply(1:4, function(k) {
    pp <- p0; pm <- p0; pp[k] <- pp[k] + h; pm[k] <- pm[k] - h
    (lli(pp, 1) - lli(pm, 1)) / (2 * h)
  }, numeric(1))
  expect_equal(unname(S[1, ]), num, tolerance = 1e-4)
})
test_that("GEV fit converges: scores sum to zero at the MLE", {
  data(itajai)
  X <- cbind(1, itajai$pressure - mean(itajai$pressure))
  f <- gevreg.fit(X, itajai$wind)
  S <- gev_scores(itajai$wind, X, f$beta, f$sigma, f$xi)
  expect_lt(max(abs(colSums(S, na.rm = TRUE))), 1e-4)
})
test_that("GEV diagnostics flag observation 82, as EVBS does", {
  data(itajai)
  X <- cbind(1, itajai$pressure - mean(itajai$pressure))
  d <- cnc_diagnostics_gev(gevreg.fit(X, itajai$wind))
  expect_identical(which.max(d$Bj), 82L)
  expect_true(82L %in% d$flagged)
})
