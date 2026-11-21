test_that("runs without error and can be printed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  te <- opsr_te(fit, type = "response")
  expect_no_error(sry_te <- summary(te))
  expect_output(print(sry_te))
})

test_that("runs with weights and produces different results", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  weights <- runif(nrow(dat))
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_w <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, weights = weights, printLevel = 0)
  te <- opsr_te(fit, type = "response")
  te_w <- opsr_te(fit_w, type = "response")
  sry_te <- summary(te)
  sry_te_w <- summary(te_w)
  expect_true(any(sry_te_w$te != sry_te$te))  # maybe even all
})

test_that("can pass weights which then produces different results", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  weights <- runif(nrow(dat))
  fit_w <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, weights = weights, printLevel = 0)
  te_w <- opsr_te(fit_w, type = "response")
  te <- opsr_te(fit_w, type = "response", weights = rep(1, nrow(dat)))
  sry_te <- summary(te)
  sry_te_w <- summary(te_w)
  expect_true(any(sry_te_w$te != sry_te$te))  # maybe even all
})
