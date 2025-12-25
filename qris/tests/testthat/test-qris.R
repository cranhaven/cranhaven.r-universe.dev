test_that("Test Smooth PE", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fm <- Surv(time, status) ~ age + sex
  set.seed(1)
  fit1 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 0, "smooth", init = double(3))
  set.seed(1)
  fit2 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "iterative", init = fit1$coef)
  set.seed(1)
  fit3 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 0, "nonsmooth", init = fit1$coef)
  expect_equal(round(fit1$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit2$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit3$coef, 3), c(5.995, -.010, .540), tolerance = .01)
})

test_that("Test pmb", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fm <- Surv(time, status) ~ age + sex
  set.seed(1)
  fit1 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "smooth", "pmb", double(3))
  set.seed(1)
  fit2 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "iterative", "pmb", fit1$coef)
  expect_equal(round(fit1$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit1$stderr, 3), c(2.404, .039, .637), tolerance = .01) 
  expect_equal(round(fit2$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit2$stderr, 3), c(2.519, .041, .787), tolerance = .01)
})

test_that("Test fmb", {
  data(cancer, package = "survival")
  lung2 <- subset(lung, select = c(time, status, age, sex))
  ## tidy up the data
  lung2$status <- lung2$status - 1
  lung2$sex <- lung2$sex - 1    
  fm <- Surv(time, status) ~ age + sex
  set.seed(1)
  fit1 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "smooth", "fmb", double(3))
  set.seed(1)
  fit2 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "iterative", "fmb", fit1$coef)
  set.seed(1)
  fit3 <- qris(fm, data = lung2, t0 = 100, Q = .5, nB = 100, "nonsmooth", "fmb", fit1$coef)
  expect_equal(round(fit1$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit1$stderr, 3), c(2.751, .044, .818), tolerance = .01)
  expect_equal(round(fit2$coef, 3), c(8.628, -.060, 1.812), tolerance = .01)
  expect_equal(round(fit2$stderr, 3), c(2.751, .044, .818), tolerance = .01)
  expect_equal(round(fit3$coef, 3), c(5.995, -.010, .540), tolerance = .01)
  expect_equal(round(fit3$stderr, 3), c(1.203, .019, .263), tolerance = .01)
})

