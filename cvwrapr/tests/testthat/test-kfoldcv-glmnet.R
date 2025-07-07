# create fake data
set.seed(1)
nobs <- 50; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)
offset <- rnorm(nobs)
penalty.factor <- rep(1:2, length.out = nvars)

test_that("basic glmnet call", {
  target_fit <- cv.glmnet(x, y, type.measure = "deviance", foldid = foldid,
                          keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call, mse", {
  target_fit <- cv.glmnet(x, y, foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call, fixed lambda sequence", {
  lambda <- c(2, 1, 0.5, 0.1, 0.05)
  target_fit <- cv.glmnet(x, y, lambda = lambda, type.measure = "deviance",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    lambda = lambda,
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights", {
  target_fit <- cv.glmnet(x, y, weights = weights, type.measure = "deviance",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    train_params = list(weights = weights),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights and offset", {
  target_fit <- cv.glmnet(x, y, weights = weights, offset = offset,
                          type.measure = "deviance",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    train_params = list(weights = weights, offset = offset),
                    predict_params = list(newoffset = offset),
                    train_row_params = c("weights", "offset"),
                    predict_row_params = c("newoffset"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights, mae", {
  target_fit <- cv.glmnet(x, y, type.measure = "mae", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    type.measure = "mae",
                    train_params = list(weights = weights),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with mix of row and non-row params", {
  target_fit <- cv.glmnet(x, y, weights = weights, type.measure = "deviance",
                          penalty.factor = penalty.factor,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    train_params = list(weights = weights,
                                        penalty.factor = penalty.factor),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})
