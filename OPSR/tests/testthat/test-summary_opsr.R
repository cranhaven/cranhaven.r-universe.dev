test_that("runs without error for basic model", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(s <- summary(fit))
  expect_output(print(s))
})

test_that("runs without error for updated model", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_updated <- update(fit, ~ . | 1)
  expect_no_error(s <- summary(fit_updated))
  expect_output(print(s))
})

test_that("runs without error for null model", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_null <- opsr_null_model(fit, printLevel = 0)
  expect_no_error(s <- summary(fit_null))
  expect_output(print(s))
})

test_that("runs without error if model fit has one fixed parameter", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  start <- opsr(f, dat, .get2step = TRUE)
  fix <- c("o2_xo2")
  start[fix] <- 0
  fit <- opsr(f, dat, start = start, fixed = fix, printLevel = 0)
  expect_no_error(s <- summary(fit))
  expect_output(print(s))
})

test_that("runs without error if model fit has multiple fixed parameters", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  start <- opsr(f, dat, .get2step = TRUE)
  fix <- c("o1_xo2", "o2_xo2")
  start[fix] <- 0
  fit <- opsr(f, dat, start = start, fixed = fix, printLevel = 0)
  expect_no_error(s <- summary(fit))
  expect_output(print(s))
})
