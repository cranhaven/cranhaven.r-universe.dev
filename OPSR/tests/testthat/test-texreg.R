test_that("basic model produces table", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(tex <- texreg::screenreg(fit))
  expect_output(print(tex))
})

test_that("basic model produces beside table", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(tex <- texreg::screenreg(fit, beside = TRUE))
  expect_output(print(tex), "Structural")
})

test_that("model comparsion produces table", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(tex <- texreg::screenreg(list(fit, fit)))
  expect_output(print(tex))
})

test_that("null model produces table", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_null <- opsr_null_model(fit, printLevel = 0)
  expect_no_error(tex <- texreg::screenreg(fit_null))
  expect_output(print(tex))
})

test_that("additional GOFs can be included", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(tex <- texreg::screenreg(fit, include.pseudoR2 = TRUE, include.R2 = TRUE))
  expect_output(print(tex))
})
