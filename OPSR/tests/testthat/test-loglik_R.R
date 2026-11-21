test_that("loglik_R and loglik_cpp produce similar results", {
  skip()  # loglik_R is slow

  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit_cpp <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_warning(fit_R <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0, .useR = TRUE))
  expect_equal(coef(fit_R), coef(fit_cpp), tolerance = 1e-1)
})
