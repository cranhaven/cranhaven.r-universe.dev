test_that("log-EVBS q and p are inverses", {
  p <- c(0.25, 0.75, 0.95)
  expect_equal(plogEVBS(qlogEVBS(p, 2.6, 0.19, -0.16), 2.6, 0.19, -0.16),
               p, tolerance = 1e-9)
})

test_that("logEVBS family reproduces evbsreg.fit on a constant model", {
  data(itajai)
  y <- log(itajai$wind); n <- length(y)
  fam <- logEVBS()
  ll <- function(par) sum(dlogEVBS(y, par[1], exp(par[2]), par[3], log = TRUE))
  gr <- function(par) {
    s <- exp(par[2])
    c(sum(fam$dldm(y, par[1], s, par[3])),
      sum(fam$dldd(y, par[1], s, par[3])) * s,
      sum(fam$dldv(y, par[1], s, par[3])))
  }
  opt <- optim(c(2.6, log(0.19), -0.16), function(p) -ll(p),
               function(p) -gr(p), method = "BFGS")
  fit <- evbsreg.fit(matrix(1, n, 1), itajai$wind)
  expect_equal(unname(opt$par[1]), unname(fit$betahat[1]), tolerance = 0.01)
  expect_equal(unname(exp(opt$par[2])), unname(fit$alphahat), tolerance = 0.01)
  expect_equal(unname(opt$par[3]), unname(fit$gamahat), tolerance = 0.01)
})

test_that("family score derivatives match numerical differentiation", {
  data(itajai)
  y <- log(itajai$wind)
  fam <- logEVBS()
  mu <- 2.6; s <- 0.19; nu <- -0.16; h <- 1e-5
  num_m <- (dlogEVBS(y, mu + h, s, nu, log = TRUE) -
            dlogEVBS(y, mu - h, s, nu, log = TRUE)) / (2 * h)
  expect_equal(fam$dldm(y, mu, s, nu), num_m, tolerance = 1e-4)
})
