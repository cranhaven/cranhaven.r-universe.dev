# create fake data
set.seed(1)
nobs <- 100; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)

# various responses
y <- rowSums(x[, 1:2]) + rnorm(nobs)
biny <- ifelse(y > 0, 1, 0)
factorbiny <- factor(biny, labels = c("a", "b"))
survy <- survival::Surv(exp(y), event = rep(c(0, 1), length.out = nobs))
multinomy <- ifelse(y > 0.6, 3, ifelse(y < -0.6, 1, 2))
factormultinomy <- factor(multinomy, labels = c("a", "b", "c"))
poiy <- exp(y)
multiy <- matrix(y, nrow = nobs, ncol = 3) + matrix(rnorm(nobs * 3), ncol = 3)

# other parameters
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)

test_that("glmnet binomial-deviance", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-deviance (factor)", {
  target_fit <- cv.glmnet(x, factorbiny, family = "binomial", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, factorbiny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-mse", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "mse",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-mae", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "mae",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-class", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "class",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "class",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-auc", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "auc",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "auc",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-auc, too few observations", {
  set.seed(3)
  foldid2 <- sample(rep(seq(12), length = nobs))
  expect_warning(target_fit <- cv.glmnet(
    x, biny, family = "binomial", weights = weights,
    type.measure = "auc", foldid = foldid2, keep = TRUE))
  expect_warning(cv_fit <- kfoldcv(
    x, biny, family = "binomial", type.measure = "auc",
    train_fun = glmnet, predict_fun = predict,
    train_params = list(family = "binomial", weights = weights),
    predict_params = list(type = "response"),
    train_row_params = c("weights"), foldid = foldid2, keep = TRUE))

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet poisson-deviance", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})

test_that("glmnet poisson-mse", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          type.measure = "mse",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, type.measure = "mse", family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})

test_that("glmnet poisson-mae", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, type.measure = "mae", family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})

test_that("glmnet cox-deviance", {
  target_fit <- cv.glmnet(x, survy, family = "cox", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, survy, family = "cox",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "cox")
})

test_that("glmnet cox-deviance, grouped = FALSE", {
  target_fit <- cv.glmnet(x, survy, family = "cox", weights = weights,
                          foldid = foldid, keep = TRUE, grouped = FALSE)
  cv_fit <- kfoldcv(x, survy, family = "cox",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE, grouped = FALSE)

  compare_glmnet_fits(target_fit, cv_fit, family = "cox")
})

test_that("glmnet cox-C", {
  target_fit <- cv.glmnet(x, survy, family = "cox", weights = weights,
                          type.measure = "C",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, survy, family = "cox", type.measure = "C",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "cox")
})

test_that("glmnet multinomial-deviance", {
  target_fit <- cv.glmnet(x, multinomy, family = "multinomial", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multinomy, family = "multinomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "multinomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "multinomial")
})

test_that("glmnet multinomial-deviance (factor)", {
  target_fit <- cv.glmnet(x, factormultinomy, family = "multinomial",
                          weights = weights, foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, factormultinomy, family = "multinomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "multinomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "multinomial")
})

test_that("glmnet multinomial-class", {
  target_fit <- cv.glmnet(x, multinomy, family = "multinomial", weights = weights,
                          type.measure = "class", foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multinomy, family = "multinomial",
                    type.measure = "class",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "multinomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "multinomial")
})

test_that("glmnet multinomial-mse", {
  target_fit <- cv.glmnet(x, multinomy, family = "multinomial", weights = weights,
                          type.measure = "mse", foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multinomy, family = "multinomial",
                    type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "multinomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "multinomial")
})

test_that("glmnet multinomial-mae", {
  target_fit <- cv.glmnet(x, multinomy, family = "multinomial", weights = weights,
                          type.measure = "mae", foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multinomy, family = "multinomial",
                    type.measure = "mae",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "multinomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "multinomial")
})

test_that("glmnet mgaussian-deviance", {
  target_fit <- cv.glmnet(x, multiy, family = "mgaussian", weights = weights,
                          type.measure = "deviance",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multiy, family = "mgaussian",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "mgaussian",
                                        weights = weights),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "mgaussian")
})

test_that("glmnet mgaussian-mse", {
  target_fit <- cv.glmnet(x, multiy, family = "mgaussian", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multiy, family = "mgaussian",
                    type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "mgaussian",
                                        weights = weights),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "mgaussian")
})

test_that("glmnet mgaussian-mae", {
  target_fit <- cv.glmnet(x, multiy, family = "mgaussian", weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, multiy, family = "mgaussian",
                    type.measure = "mae",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "mgaussian",
                                        weights = weights),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "mgaussian")
})

test_that("glmnet binomial probit-deviance", {
  family <- binomial(link = "probit")
  target_fit <- cv.glmnet(x, biny, family = family, weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = family,
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = family,
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = family)
})

test_that("glmnet binomial probit-mse", {
  family <- binomial(link = "probit")
  target_fit <- cv.glmnet(x, biny, family = family, weights = weights,
                          type.measure = "mse",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = family,
                    type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = family,
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = family)
})

test_that("glmnet binomial probit-mae", {
  family <- binomial(link = "probit")
  target_fit <- cv.glmnet(x, biny, family = family, weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = family,
                    type.measure = "mae",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = family,
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = family)
})
