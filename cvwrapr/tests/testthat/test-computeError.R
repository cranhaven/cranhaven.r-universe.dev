# create fake data
set.seed(10)
nobs <- 100; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
survy <- survival::Surv(exp(y), event = rep(c(0, 1), length.out = nobs))

# other parameters
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)

test_that("family='cox' && type.measure='deviance' && grouped=FALSE", {
  cv_fit <- kfoldcv(x, survy, family = "cox",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE, grouped = FALSE)

  predmat <- cv_fit$fit.preval
  attr(predmat, "cvraw") <- NULL
  err <- computeError(predmat, survy, cv_fit$lambda, foldid,
                      type.measure = "deviance", family = "cox",
                      weights = weights, grouped = FALSE)

  expect_equal(cv_fit$lambda, err$lambda)
  expect_equal(cv_fit$cvm, err$cvm)
  expect_equal(cv_fit$cvsd, err$cvsd)
  expect_equal(cv_fit$cvup, err$cvup)
  expect_equal(cv_fit$cvlo, err$cvlo)
})

test_that("family='cox' && type.measure='deviance' && grouped=TRUE", {
  cv_fit <- kfoldcv(x, survy, family = "cox",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE, grouped = TRUE)

  predmat <- cv_fit$fit.preval
  attr(predmat, "cvraw") <- NULL
  expect_error(computeError(predmat, survy, cv_fit$lambda, foldid,
                            type.measure = "deviance", family = "cox",
                            weights = weights, grouped = TRUE))
})
