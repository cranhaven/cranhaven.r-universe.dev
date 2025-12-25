test_that("Test additional data from plot", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fit <- qris(Surv(time, status) ~ age + sex, data = lung2,
              t0 = 100, Q = .5, nB = 0, "smooth", init = double(3))
  fit <- qris.extend(fit, Qs = 3:6 / 10, t0s = 1:6 * 10)
  expect_equal(fit$varNames, c("(Intercept)", "age", "sex"))
  expect_equal(all(!is.na(fit$ggdat$Est)), TRUE)
  expect_equal(unique(fit$ggdat$Qs), 3:6 / 10)
  expect_equal(unique(fit$ggdat$t0s), 1:6 * 10)
})



