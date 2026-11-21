test_that("basic model converges and can recover ground truth", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_equal(fit$code, 0)
  ground_truth <- sim_dat$params
  sig <- sim_dat$sigma
  sigma <- diag(sig)[2:ncol(sig)]
  rho <- sig[1, 2:ncol(sig)]
  gamma <- unname(coef(fit, component = "selection"))
  beta <- unname(coef(fit, component = "outcome"))
  structural <- unname(coef(fit, component = "structural"))
  expect_equal(gamma, ground_truth$gamma, tolerance = 1e-1)
  expect_equal(beta, c(ground_truth$beta1, ground_truth$beta2, ground_truth$beta3), tolerance = 1e-1)
  expect_equal(structural, c(ground_truth$kappa, sigma, rho), tolerance = 1e-1)
})

test_that("converges for method NM and BFGS", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit_bfgs <- opsr(f, dat, printLevel = 0)
  fit_nm <- opsr(f, dat, printLevel = 0, method = "NM")
  expect_equal(coef(fit_bfgs), coef(fit_nm), tolerance = 1e-1)
})

test_that("starting values can be passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  start <- opsr(f, dat, .get2step = TRUE)
  expect_no_error(opsr(f, dat, start = start, printLevel = 0))
  expect_error(opsr(f, dat, start = c(1, 2, 3)), regexp = "Start vector 'start' has wrong length.")
})

test_that("parameters can be fixed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  start <- opsr(f, dat, .get2step = TRUE)
  fix <- c("rho1", "rho2", "rho3")
  start[fix] <- 0
  fit <- opsr(f, dat, start = start, fixed = fix, printLevel = 0)
  test <- unname(coef(fit)[fix])
  expect_true(all(test == 0))
})

test_that("accepts usual formula syntax (factors and transformations) in outcome process", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- ifelse(dat$xo2 > 0, "a", "b")
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + factor(xo3), data = dat, printLevel = 0))
  expect_equal(fit$code, 0)
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + I(xo2**2) + log(xo1 + 10), data = dat, printLevel = 0))
  expect_equal(fit$code, 0)
})

test_that("accepts usual formula syntax (factors and transformations in selection process", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xs3 <- ifelse(dat$xs2 > 0, "a", "b")
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + factor(xs3) | xo1 + xo2, data = dat, printLevel = 0))
  expect_equal(fit$code, 0)
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + I(xs2**2) + log(xs1 + 10) | xo1 + xo2, data = dat, printLevel = 0))
  expect_equal(fit$code, 0)
})

test_that("accepts weights vector", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  w1 <- rep(1, nrow(dat))
  w2 <- runif(nrow(dat))
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit <- opsr(f, dat, printLevel = 0)
  fit1 <- opsr(f, dat, weights = w1, printLevel = 0)
  expect_equal(coef(fit1), coef(fit))
  fit2 <- opsr(f, dat, weights = w2, printLevel = 0)
  expect_false(any(coef(fit2) == coef(fit1)))
})

test_that("accepts subset", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  sub <- c(rep(TRUE, 100), rep(FALSE, nrow(dat) - 100))
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, subset = sub, printLevel = 0))
  expect_equal(nobs(fit), 100)
})

test_that("runs on machines without OpenMP", {
  skip_if(opsr_check_omp())

  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0))
})

test_that("works if data is not passed (finds in env)", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  ys <- dat$ys
  yo <- dat$yo
  xs1 <- dat$xs1
  xs2 <- dat$xs2
  xo1 <- dat$xo1
  xo2 <- dat$xo2
  rm(dat)
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  expect_no_error(opsr(f, printLevel = 0))
})

test_that("log-likelihood values are returned in correct order", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat[1, "yo"] <- -100
  dat[5, "yo"] <- -100
  dat[20, "yo"] <- -100
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit <- opsr(f, dat, printLevel = 0)
  ll <- fit$loglik(fit$estimate)
  expect_true(all(which(ll < -50) == c(1, 5, 20)))
})

test_that(".loglik, fit$loglik() and loglik_cpp all produce identical vectors", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit <- opsr(f, dat, printLevel = 0)
  ll1 <- opsr(f, dat, start = fit$estimate, .loglik = TRUE)
  ll2 <- fit$loglik(fit$estimate)
  mm <- model.matrix(fit)
  oZ <- order(dat$ys)
  Y <- lapply(sort(unique(dat$ys)), function(z) {
    dat$yo[dat$ys == z]
  })
  ll3 <- loglik_cpp(fit$estimate, mm$W, mm$X, Y, fit$weights, 3, 1)[order(oZ)]
  expect_true(all(ll1 == ll2))
  expect_true(all(ll1 == ll3))
})

test_that("warns on singularity issues", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- dat$xo2
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2 + xo3
  expect_warning(opsr(f, dat, printLevel = 0))
  expect_no_warning(opsr(f, dat, printLevel = 0, fixed = "o3_xo3"))
})

test_that("runs if treatment is binary", {
  sim_dat <- load_sim_dat()
  dat <- subset(sim_dat$data, subset = ys %in% c(1, 2))
  expect_error(MASS::polr(factor(ys) ~ xs1 + xs2, data = dat, method = "probit"))
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0))
  expect_equal(fit$code, 0)
})

test_that("runs if more than 3 treatment regimes", {
  sim_dat <- sim4()
  dat <- sim_dat$dat
  expect_no_error(fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0))
  expect_equal(fit$code, 0)
  ground_truth <- sim_dat$params
  sig <- sim_dat$sigma
  sigma <- diag(sig)[2:ncol(sig)]
  rho <- sig[1, 2:ncol(sig)]
  gamma <- unname(coef(fit, component = "selection"))
  beta <- unname(coef(fit, component = "outcome"))
  structural <- unname(coef(fit, component = "structural"))
  expect_equal(gamma, ground_truth$gamma, tolerance = 3e-1)
  expect_equal(beta, c(ground_truth$beta1, ground_truth$beta2, ground_truth$beta3, ground_truth$beta4), tolerance = 3e-1)
  expect_equal(structural, c(ground_truth$kappa, sigma, rho), tolerance = 3e-1)
})
