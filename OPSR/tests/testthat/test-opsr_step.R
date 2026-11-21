skip()

test_that("runs without error and drops some terms", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  expect_no_error(fit_step <- opsr_step(fit, pval = 0.1, printLevel = 0))
  expect_true(length(coef(fit_step)) < length(coef(fit)))
})
