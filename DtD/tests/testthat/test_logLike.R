context("Testing log-likelihood evaluation")

test_that("We get the same with optim whether we use build in function or optim with 'merton_ll'", {
  set.seed(4648394)
  sims <- BS_sim(
    vol = .1, mu = .05, dt = .1, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)

  eps <- 1e-10
  r1 <- with(
    sims, BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle",
                 eps = eps, vol_start = .2))

  r2 <- optim(c(mu = 0, log_vol = log(.2)), function(par)
    -with(
      sims, merton_ll(S = S, D = D, T. = T, r = r, time = time,
                      mu = par["mu"], vol = exp(par["log_vol"]))),
    control = list(reltol = eps))

  # BS_fit uses a profile likelihood function and thus it may not give
  # exaclty the same
  expect_equal(r1$ests[1], r2$par[1], tolerance = eps^(1/2))
  expect_equal(r1$ests[2], exp(r2$par[2]), check.attributes = FALSE,
               tolerance = eps^(1/2))

  ll_optim <- -r2$value
  ll_bs_fit <- with(
    sims, merton_ll(S = S, D = D, T. = T, r = r, time = time,
                   mu = r1$ests[1], vol = r1$ests[2]))

  expect_equal(ll_bs_fit, ll_optim, tolerance = eps^(4/5))
})
