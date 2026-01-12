context("Testing scrnp functions")

test_that("scrnp returns numerics", {
  set.seed(123)
  n <- 100
  p <- 1
  X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
  Y <- rbinom(n, 1, plogis(0.2 * X[,1]))
  # try without nested cv
  set.seed(123)
  fit1 <- cv_scrnp(Y = Y, X = data.frame(X), K = 3, nested_cv = FALSE, learner = "glm_wrapper")
  # try with nested cv
  set.seed(123)
  fit2 <- cv_scrnp(Y = Y, X = data.frame(X), K = 3, nested_cv = TRUE, nested_K = 2, 
                      learner = "glm_wrapper")
  expect_equal(fit1$est_empirical, fit2$est_empirical)
  expect_true(is.numeric(fit1$est_cvtmle))
  expect_true(!is.na(fit1$est_cvtmle))
  expect_true(is.numeric(fit2$est_cvtmle))
  expect_true(!is.na(fit2$est_cvtmle))
})
