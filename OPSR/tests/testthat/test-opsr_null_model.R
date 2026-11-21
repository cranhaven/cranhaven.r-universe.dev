test_that("basic null model API works", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(fit_null <- opsr_null_model(fit, printLevel = 0))
  expect_no_error(s <- summary(fit_null))
  expect_output(print(s))
})

test_that("formula operators are allowed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + I(xo2), dat, printLevel = 0)
  expect_no_error(fit_null <- opsr_null_model(fit, printLevel = 0))
  expect_no_error(s <- summary(fit_null))
  expect_output(print(s))
})
