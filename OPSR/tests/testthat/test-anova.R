test_that("compares against null modell", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(a <- anova(fit))
  expect_output(print(a))
  expect_equal(nrow(a$table), 2)
  expect_match(deparse(a$formulas[[1]]), "~Nullmodel")
  expect_lt(a$table[1, "logLik"], a$table[2, "logLik"])
})

test_that("model comparison works", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_inter <- update(fit, ~ . | 1)
  expect_no_error(a <- anova(fit_inter, fit))
  expect_output(print(a))
  expect_equal(nrow(a$table), 2)
  expect_lt(a$table[1, "logLik"], a$table[2, "logLik"])
})

test_that("null model can be explicitly passed in is correctly rejected", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_null <- opsr_null_model(fit, printLevel = 0)
  expect_no_error(a <- anova(fit_null, fit))
  expect_output(print(a))
  expect_equal(a$table[2, "Pr(>Chi)"], 0, tolerance = 1e-1)
})
