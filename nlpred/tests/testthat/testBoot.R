context("Testing bootstrap functions")

test_that("auc boot functions", {
  set.seed(123)
  n <- 100
  p <- 1
  X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
  Y <- rbinom(n, 1, plogis(0.2 * X[,1]))

  boot1 <- boot_auc(Y = Y, X = X, B = 10)
  boot2 <- boot_auc(Y = Y, X = X, B = 10, correct632 = TRUE)
  lpo <- lpo_auc(Y = Y, X = X, max_pairs = 10)
  expect_true(is.numeric(boot1$auc))
  expect_true(is.numeric(boot2$auc))
  expect_true(is.numeric(lpo$auc))
  expect_true(boot1$auc >= 0 & boot1$auc <= 1)
  expect_true(boot2$auc >= 0 & boot2$auc <= 1)
  expect_true(lpo$auc >= 0 & lpo$auc <= 1)
})

test_that("scrnp boot functions", {
  set.seed(123)
  n <- 100
  p <- 1
  X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
  Y <- rbinom(n, 1, plogis(0.2 * X[,1]))

  boot1 <- boot_scrnp(Y = Y, X = X, B = 10)
  boot2 <- boot_scrnp(Y = Y, X = X, B = 10, correct632 = TRUE)
  expect_true(is.numeric(boot1$scrnp))
  expect_true(is.numeric(boot2$scrnp))
})




