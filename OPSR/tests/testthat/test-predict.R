test_that("all type args produce some output", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(predict(fit, group = 1, type = "response"))
  expect_no_error(predict(fit, group = 1, counterfact = 2, type = "response"))
  expect_no_error(predict(fit, group = 1, type = "unlog-response"))
  expect_no_error(predict(fit, group = 1, counterfact = 2, type = "unlog-response"))
  expect_no_error(predict(fit, group = 1, type = "prob"))
  expect_no_error(predict(fit, group = 1, counterfact = 2, type = "prob"))
  expect_no_error(predict(fit, group = 1, type = "mills"))
  expect_no_error(predict(fit, group = 1, counterfact = 2, type = "mills"))
})

test_that("works on intercept-only model (continuous outcome)", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | 1, dat, printLevel = 0)
  expect_no_error(predict(fit, group = 1))
  expect_no_error(predict(fit, group = 2))
  expect_no_error(predict(fit, group = 3))
  expect_no_error(predict(fit, group = 1, counterfact = 3))
  expect_no_error(predict(fit, group = 1, type = "prob"))
  expect_no_error(predict(fit, group = 1, type = "unlog-response"))
})

test_that("can predict on a single new data point", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  group <- 1
  newdat <- dat[dat$ys == group, ][1, ]
  expect_no_error(p <- predict(fit, group = group, newdata = newdat))
})

test_that("can predict on new data points", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  newdat <- dat[1:4, ]
  expect_error(predict(fit, group = 3, newdata = newdat))
  expect_no_error(predict(fit, group = 2, newdata = newdat))
})

test_that("produces numeric vector of length n for all type args", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  n <- nobs(fit)
  p <- predict(fit, group = 1, type = "response")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, counterfact = 2, type = "response")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, type = "unlog-response")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, counterfact = 2, type = "unlog-response")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, type = "prob")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, counterfact = 2, type = "prob")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, type = "mills")
  expect_length(p, n)
  expect_type(p, "double")
  p <- predict(fit, group = 1, counterfact = 2, type = "mills")
  expect_length(p, n)
  expect_type(p, "double")
})

test_that("NAs across regimes align", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  p1 <- predict(fit, group = 1)
  p2 <- predict(fit, group = 2)
  p3 <- predict(fit, group = 3)
  p_sum <- rowSums(cbind(p1, p2, p3), na.rm = TRUE)
  p_max <- pmax(p1, p2, p3, na.rm = TRUE)
  expect_true(all(p_sum == p_max))
})

test_that("probs for selection process sum to 1", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  p1 <- predict(fit, group = 1, counterfact = 1, type = "prob")
  p2 <- predict(fit, group = 1, counterfact = 2, type = "prob")
  p3 <- predict(fit, group = 1, counterfact = 3, type = "prob")
  p_sum <- round(rowSums(na.omit(cbind(p1, p2, p3))), digits = 3)
  expect_true(all(p_sum == 1))
})

test_that("yields similar results to predict.lm if no error correlation", {
  sigma <- diag(1, nrow = 4, ncol = 4)  # no cor
  sim_dat <- opsr_simulate(sigma = sigma)
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, data = dat, printLevel = 0)
  group <- 1
  idx <- dat$ys == group
  p_opsr <- predict(fit, group = group)
  fit_lm <- lm(yo ~ xo1 + xo2, data = dat, subset = idx)
  p_lm <- predict(fit_lm)
  df <- data.frame(p_opsr = na.omit(p_opsr), p_lm = p_lm)
  test <- unname(coef(lm(p_lm ~ -1 + p_opsr, data = df)))
  expect_equal(test, 1, tolerance = 1e-2)
})

test_that("works on model with formula transformation (factor)", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xs3 <- ifelse(dat$xs2 > 0, "a", "b")
  fit <- opsr(ys | yo ~ xs1 + factor(xs3) | xo1 + xo2, data = dat, printLevel = 0)
  expect_no_error(predict(fit, group = 1, type = "prob"))
})
