context("Testing AUC functions")

test_that("AUC returns numerics", {
  set.seed(123)
  n <- 100
  p <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- rbinom(n, 1, plogis(0.2 * X[,1]))
  # try without nested cv
  set.seed(123)
  fit1 <- cv_auc(Y = Y, X = data.frame(X), K = 3, nested_cv = FALSE, learner = "glm_wrapper")
  # try with nested cv
  set.seed(123)
  fit2 <- cv_auc(Y = Y, X = data.frame(X), K = 3, nested_cv = TRUE, nested_K = 2, 
                      learner = "glm_wrapper")
  # try with nested cv with nested_K != K - 1
  set.seed(123)
  fit3 <- cv_auc(Y = Y, X = data.frame(X), K = 3, nested_cv = TRUE, nested_K = 3, 
                      learner = "glm_wrapper")
  expect_equal(fit1$est_empirical, fit2$est_empirical)
  expect_true(is.numeric(fit1$est_cvtmle))
  expect_true(!is.na(fit1$est_cvtmle))
  expect_true(is.numeric(fit2$est_cvtmle))
  expect_true(!is.na(fit2$est_cvtmle))
  expect_true(is.numeric(fit3$est_cvtmle))
  expect_true(!is.na(fit3$est_cvtmle))
})

# test_that("AUC returns 0.5 for SL.mean", {
#   mySL.mean <- function(train, test){
#     ntrain <- length(train$Y)
#     ntest <- length(test$Y)
#     mtrain_y <- mean(train$Y)
#     return(list(train_y = train$Y, test_y = test$Y,
#            train_pred = rep(mtrain_y, ntrain),
#            test_pred = rep(mtrain_y, ntest)))
#   }
#   set.seed(123)
#   n <- 50
#   p <- 1
#   X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#   Y <- rbinom(n, 1, plogis(X[,1]))
#   # try without nested cv
#   set.seed(123)
#   fit1 <- cv_auc(Y = Y, X = data.frame(X), K = 3, nested_cv = FALSE, learner = "mySL.mean")
#   # try with nested cv
#   set.seed(123)
#   fit2 <- cv_auc(Y = Y, X = data.frame(X), K = 3, nested_cv = TRUE, nested_K = 2, 
#                       learner = "mySL.mean")
#   expect_equal(fit1$est_cvtmle, 0.5)
#   expect_equal(fit1$est_empirical, 0.5)
#   expect_equal(fit2$est_cvtmle, 0.5)
# })

