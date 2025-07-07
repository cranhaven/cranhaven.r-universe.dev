# create fake data
set.seed(1)
nobs <- 50; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
biny <- ifelse(y > 0, 1, 0)
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)

test_that("foldid need not be 1:nfolds", {
  cv_fit1 <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                     foldid = foldid, keep = TRUE)
  cv_fit2 <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                     foldid = -foldid * 2, keep = TRUE)

  compare_glmnet_fits(cv_fit1, cv_fit2)
})

test_that("foldid need not be 1:nfolds, auc", {
  cv_fit1 <- kfoldcv(x, biny, type.measure = "auc", family = "binomial",
                     train_fun = glmnet, predict_fun = predict,
                     train_params = list(family = "binomial"),
                     predict_params = list(type = "response"),
                     foldid = foldid, keep = TRUE)
  cv_fit2 <- kfoldcv(x, biny, type.measure = "auc", family = "binomial",
                     train_fun = glmnet, predict_fun = predict,
                     train_params = list(family = "binomial"),
                     predict_params = list(type = "response"),
                     foldid = -foldid * 2, keep = TRUE)

  compare_glmnet_fits(cv_fit1, cv_fit2)
})

test_that("invalid type.measure", {
  expect_error(kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                       type.measure = "class"))
})

test_that("family mismatch", {
  expect_warning(kfoldcv(x, biny, train_fun = glmnet, predict_fun = predict,
                         type.measure = "deviance",
                         train_params = list(family = "binomial")))
})

test_that("parallel", {
  cl <- parallel::makeCluster(2)
  doParallel::registerDoParallel(cl)

  cv_fit1 <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                     train_params = list(weights = weights),
                     train_row_params = c("weights"),
                     foldid = foldid, keep = TRUE)
  cv_fit2 <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                     train_params = list(weights = weights),
                     train_row_params = c("weights"),
                     foldid = foldid, keep = TRUE, parallel = TRUE)

  parallel::stopCluster(cl)

  compare_glmnet_fits(cv_fit1, cv_fit2)
})
