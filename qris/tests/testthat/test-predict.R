test_that("Test predict", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fm <- Surv(time, status) ~ age + sex
  set.seed(1)
  fit1 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 0, "smooth", init = double(3))
  fitted1 <- predict(fit1)
  fitted2 <- predict(fit1, data.frame(age = 0, sex = 0))  
  fitted3 <- predict(fit1, data.frame(age = 1, sex = 1))  
  expect_equal(as.numeric(quantile(fitted1, 0:5 / 5)),
               c(140.2967, 193.5368, 251.3428, 475.9269, 921.6643, 3005.7914),
               tolerance = .01)
  expect_equal(as.numeric(fitted2), exp(fit1$coef[1]) + fit1$para$t0)
  expect_equal(as.numeric(fitted3), exp(sum(fit1$coef)) + fit1$para$t0)
})

test_that("Test residuals", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fm <- Surv(time, status) ~ age + sex
  set.seed(1)
  fit1 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 0, "smooth", init = double(3))
  resid1 <- resid(fit1)
  resid2 <- resid(fit1, data.frame(time = 1000, age = 0, sex = 0))
  resid3 <- resid(fit1, data.frame(time = 1000, age = 1, sex = 1))
  expect_equal(as.numeric(quantile(resid1, 0:5 / 5)),
               c(-856.79988, -98.77243, 49.06627, 174.52981, 732.32219, 2813.79137),
               tolerance = .01)  
  expect_equal(as.numeric(resid2), exp(fit1$coef[1]) + fit1$para$t0 - 1000)
  expect_equal(as.numeric(resid3), exp(sum(fit1$coef)) + fit1$para$t0 - 1000)
})
