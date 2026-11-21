test_that("runs without error", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(te <- opsr_te(fit, type = "response"))
})
