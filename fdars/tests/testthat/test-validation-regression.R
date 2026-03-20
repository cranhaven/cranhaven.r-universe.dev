# Validation tests for regression functions

test_that("fregre.pc produces valid predictions", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid * (1 + 0.2 * i/n)) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.pc(fd, y, ncomp = 3)

  expect_s3_class(model, "fregre.fd")
  expect_length(model$fitted.values, n)
  expect_length(model$residuals, n)

  # Fitted + residuals should equal y
  expect_equal(model$fitted.values + model$residuals, y, tolerance = 1e-10)
})

test_that("fregre.basis produces valid predictions", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.basis(fd, y, nbasis = 10)

  expect_s3_class(model, "fregre.fd")
  expect_length(model$fitted.values, n)
  expect_length(model$residuals, n)
})

test_that("fregre.np produces valid predictions", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.np(fd, y, h = 0.5)

  expect_s3_class(model, "fregre.np")
  expect_length(model$fitted.values, n)
  expect_length(model$residuals, n)
})

test_that("fregre.pc.cv selects optimal ncomp", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid * (1 + 0.2 * i/n)) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  cv_result <- fdars::fregre.pc.cv(fd, y, kfold = 5, ncomp.range = 1:5, seed = 123)

  expect_true("optimal.ncomp" %in% names(cv_result))
  expect_true("cv.errors" %in% names(cv_result))
  expect_true("model" %in% names(cv_result))

  expect_gte(cv_result$optimal.ncomp, 1)
  expect_lte(cv_result$optimal.ncomp, 5)
  expect_length(cv_result$cv.errors, 5)
})

test_that("fregre.basis.cv selects optimal lambda", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  lambdas <- c(0, 0.01, 0.1, 1)
  cv_result <- fdars::fregre.basis.cv(fd, y, kfold = 5, lambda.range = lambdas, seed = 123)

  expect_true("optimal.lambda" %in% names(cv_result))
  expect_true("cv.errors" %in% names(cv_result))
  expect_true("model" %in% names(cv_result))

  expect_true(cv_result$optimal.lambda %in% lambdas)
  expect_length(cv_result$cv.errors, length(lambdas))
})

test_that("fregre.np.cv selects optimal bandwidth", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  h_range <- c(0.1, 0.5, 1, 2)
  cv_result <- fdars::fregre.np.cv(fd, y, kfold = 5, h.range = h_range, seed = 123)

  expect_true("optimal.h" %in% names(cv_result))
  expect_true("cv.errors" %in% names(cv_result))
  expect_true("model" %in% names(cv_result))

  expect_true(cv_result$optimal.h %in% h_range)
  expect_length(cv_result$cv.errors, length(h_range))
})

# =============================================================================
# Predict Method Tests
# =============================================================================

test_that("predict.fregre.fd works for fregre.pc", {
  set.seed(123)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  # Split into train/test
  train_idx <- 1:20
  test_idx <- 21:30
  fd_train <- fdars::fdata(X[train_idx, ], argvals = t_grid)
  fd_test <- fdars::fdata(X[test_idx, ], argvals = t_grid)
  y_train <- y[train_idx]
  y_test <- y[test_idx]

  # Fit model
  fit <- fdars::fregre.pc(fd_train, y_train, ncomp = 3)

  # Test predict with new data
  pred <- predict(fit, fd_test)
  expect_length(pred, length(test_idx))
  expect_true(is.numeric(pred))

  # Test predict without new data returns fitted values
  pred_fitted <- predict(fit)
  expect_equal(pred_fitted, fit$fitted.values)
})

test_that("predict.fregre.fd works for fregre.basis", {
  set.seed(123)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  # Split into train/test
  train_idx <- 1:20
  test_idx <- 21:30
  fd_train <- fdars::fdata(X[train_idx, ], argvals = t_grid)
  fd_test <- fdars::fdata(X[test_idx, ], argvals = t_grid)
  y_train <- y[train_idx]

  # Fit model
  fit <- fdars::fregre.basis(fd_train, y_train, lambda = 0.1)

  # Test predict with new data
  pred <- predict(fit, fd_test)
  expect_length(pred, length(test_idx))
  expect_true(is.numeric(pred))

  # Test predict without new data returns fitted values
  pred_fitted <- predict(fit)
  expect_equal(pred_fitted, fit$fitted.values)
})

test_that("predict.fregre.np works", {
  set.seed(123)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  # Split into train/test
  train_idx <- 1:20
  test_idx <- 21:30
  fd_train <- fdars::fdata(X[train_idx, ], argvals = t_grid)
  fd_test <- fdars::fdata(X[test_idx, ], argvals = t_grid)
  y_train <- y[train_idx]

  # Fit model
  fit <- fdars::fregre.np(fd_train, y_train)

  # Test predict with new data
  pred <- predict(fit, fd_test)
  expect_length(pred, length(test_idx))
  expect_true(is.numeric(pred))

  # Test predict without new data returns fitted values
  pred_fitted <- predict(fit)
  expect_equal(pred_fitted, fit$fitted.values)
})

test_that("predict accepts matrix input and converts to fdata", {
  set.seed(123)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  fit <- fdars::fregre.pc(fd, y, ncomp = 3)

  # Predict with raw matrix (should be converted to fdata internally)
  X_new <- matrix(sin(2 * pi * t_grid) * 0.5, nrow = 1)
  pred <- predict(fit, X_new)

  expect_length(pred, 1)
  expect_true(is.numeric(pred))
})

# =============================================================================
# k-NN Nonparametric Regression Tests
# =============================================================================

test_that("fregre.np with kNN.gCV produces valid results", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.np(fd, y, type.S = "kNN.gCV", knn = 20)

  expect_s3_class(model, "fregre.np")
  expect_equal(model$type.S, "kNN.gCV")
  expect_length(model$fitted.values, n)
  expect_length(model$residuals, n)
  expect_length(model$k.opt, 1)  # Global: single k
  expect_gte(model$k.opt, 1)
  expect_lte(model$k.opt, 20)
})

test_that("fregre.np with kNN.lCV produces valid results", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.np(fd, y, type.S = "kNN.lCV", knn = 20)

  expect_s3_class(model, "fregre.np")
  expect_equal(model$type.S, "kNN.lCV")
  expect_length(model$fitted.values, n)
  expect_length(model$residuals, n)
  expect_length(model$k.opt, n)  # Local: one k per observation
  expect_true(all(model$k.opt >= 1))
  expect_true(all(model$k.opt <= 20))
})

test_that("predict.fregre.np works with kNN models", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)

  # Test with global CV
  fit_gcv <- fdars::fregre.np(fd, y, type.S = "kNN.gCV", knn = 15)

  # Create new data with correct dimensions (2 rows, m columns)
  X_new <- matrix(0, 2, m)
  X_new[1, ] <- sin(2 * pi * t_grid) * 0.5 + rnorm(m, sd = 0.1)
  X_new[2, ] <- sin(2 * pi * t_grid) * 0.3 + rnorm(m, sd = 0.1)
  fd_new <- fdars::fdata(X_new, argvals = t_grid)

  pred_gcv <- predict(fit_gcv, fd_new)
  expect_length(pred_gcv, 2)
  expect_true(is.numeric(pred_gcv))

  # Test with local CV
  fit_lcv <- fdars::fregre.np(fd, y, type.S = "kNN.lCV", knn = 15)
  pred_lcv <- predict(fit_lcv, fd_new)
  expect_length(pred_lcv, 2)
  expect_true(is.numeric(pred_lcv))

  # Predict without new data should return fitted values
  expect_equal(predict(fit_gcv), fit_gcv$fitted.values)
  expect_equal(predict(fit_lcv), fit_lcv$fitted.values)
})

# =============================================================================
# fregre.np.multi Tests
# =============================================================================

test_that("fregre.np.multi with equal weights works", {
  set.seed(42)
  n <- 40
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(0, n, m)
  X2 <- matrix(0, n, m)
  for (i in 1:n) {
    X1[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
    X2[i, ] <- cos(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X1) + 0.5 * rowMeans(X2) + rnorm(n, sd = 0.1)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  fit <- fdars::fregre.np.multi(list(fd1, fd2), y)

  expect_s3_class(fit, "fregre.np.multi")
  expect_length(fit$fitted.values, n)
  expect_length(fit$residuals, n)
  expect_equal(fit$weights, c(0.5, 0.5))
  expect_equal(length(fit$D.list), 2)
})

test_that("fregre.np.multi with fixed weights works", {
  set.seed(42)
  n <- 40
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  fit <- fdars::fregre.np.multi(list(fd1, fd2), y, weights = c(0.7, 0.3))

  expect_equal(fit$weights, c(0.7, 0.3))
})

test_that("fregre.np.multi with CV weights works", {
  set.seed(42)
  n <- 40
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(0, n, m)
  X2 <- matrix(0, n, m)
  for (i in 1:n) {
    X1[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
    X2[i, ] <- cos(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X1) + 0.5 * rowMeans(X2) + rnorm(n, sd = 0.1)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  fit <- fdars::fregre.np.multi(list(fd1, fd2), y, weights = "cv", cv.folds = 3)

  expect_s3_class(fit, "fregre.np.multi")
  expect_true(!is.null(fit$weights.cv))
  expect_equal(sum(fit$weights), 1)
  expect_true(all(fit$weights >= 0))
})

test_that("fregre.np.multi with kNN works", {
  set.seed(42)
  n <- 40
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  fit_gcv <- fdars::fregre.np.multi(list(fd1, fd2), y, type.S = "kNN.gCV", knn = 15)
  fit_lcv <- fdars::fregre.np.multi(list(fd1, fd2), y, type.S = "kNN.lCV", knn = 15)

  expect_s3_class(fit_gcv, "fregre.np.multi")
  expect_s3_class(fit_lcv, "fregre.np.multi")
  expect_true(!is.null(fit_gcv$k.opt))
  expect_true(!is.null(fit_lcv$k.opt))
})

test_that("fregre.np.multi print method works", {
  set.seed(42)
  n <- 30
  m <- 15
  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1)
  fd2 <- fdars::fdata(X2)

  fit <- fdars::fregre.np.multi(list(fd1, fd2), y)

  expect_output(print(fit), "multiple predictors")
  expect_output(print(fit), "Weights:")
  expect_output(print(fit), "R-squared:")
})

test_that("fregre.np.multi predict method works", {
  set.seed(42)
  n <- 40
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  fit <- fdars::fregre.np.multi(list(fd1, fd2), y)

  # Predict without new data returns fitted values
  expect_equal(predict(fit), fit$fitted.values)

  # Predict with new data
  X1_new <- matrix(rnorm(5 * m), 5, m)
  X2_new <- matrix(rnorm(5 * m), 5, m)
  fd1_new <- fdars::fdata(X1_new, argvals = t_grid)
  fd2_new <- fdars::fdata(X2_new, argvals = t_grid)

  pred <- predict(fit, list(fd1_new, fd2_new))
  expect_length(pred, 5)
  expect_true(is.numeric(pred))
})

test_that("fregre.np.multi with three predictors works", {
  set.seed(42)
  n <- 40
  m <- 15

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  X3 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1)
  fd2 <- fdars::fdata(X2)
  fd3 <- fdars::fdata(X3)

  fit <- fdars::fregre.np.multi(list(fd1, fd2, fd3), y)

  expect_s3_class(fit, "fregre.np.multi")
  expect_length(fit$weights, 3)
  expect_equal(sum(fit$weights), 1)
})

test_that("fregre.np.multi rejects mismatched inputs", {
  n <- 30
  m <- 15

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm((n + 5) * m), n + 5, m)  # Different n
  y <- rnorm(n)

  fd1 <- fdars::fdata(X1)
  fd2 <- fdars::fdata(X2)

  expect_error(fdars::fregre.np.multi(list(fd1, fd2), y), "same number of observations")
})

test_that("fregre.np.multi rejects wrong y length", {
  n <- 30
  m <- 15

  X1 <- matrix(rnorm(n * m), n, m)
  X2 <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n + 5)  # Wrong length

  fd1 <- fdars::fdata(X1)
  fd2 <- fdars::fdata(X2)

  expect_error(fdars::fregre.np.multi(list(fd1, fd2), y), "Length of y")
})

# =============================================================================
# Print Method Tests
# =============================================================================

test_that("print.fregre.fd produces output", {
  set.seed(42)
  n <- 30
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)
  fd <- fdars::fdata(X, argvals = t_grid)

  fit <- fdars::fregre.pc(fd, y, ncomp = 3)
  expect_output(print(fit), "Functional regression")
  expect_output(print(fit), "R-squared")
})

test_that("print.fregre.np produces output", {
  set.seed(42)
  n <- 30
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)
  fd <- fdars::fdata(X, argvals = t_grid)

  fit <- fdars::fregre.np(fd, y)
  expect_output(print(fit), "Nonparametric")
  expect_output(print(fit), "R-squared")
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("fregre.pc with ncomp=NULL auto-selects components", {
  set.seed(42)
  n <- 50
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
  }
  y <- rowMeans(X) + rnorm(n, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  model <- fdars::fregre.pc(fd, y)  # ncomp = NULL

  expect_s3_class(model, "fregre.fd")
  expect_true(model$ncomp >= 1)
  expect_length(model$fitted.values, n)
})

test_that("fregre.basis with lambda > 0 vs lambda = 0", {
  set.seed(42)
  n <- 50
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)
  fd <- fdars::fdata(X, argvals = t_grid)

  fit_no_reg <- fdars::fregre.basis(fd, y, lambda = 0)
  fit_reg <- fdars::fregre.basis(fd, y, lambda = 10)

  # Regularization should shrink coefficients
  expect_true(sum(fit_reg$coefficients^2) < sum(fit_no_reg$coefficients^2))
})

test_that("fregre.basis with mismatched y length errors", {
  n <- 30
  m <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X)

  expect_error(fdars::fregre.basis(fd, rnorm(n + 5)), "Length of y")
})

test_that("fregre.pc with mismatched y length errors", {
  n <- 30
  m <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X)

  expect_error(fdars::fregre.pc(fd, rnorm(n + 5)), "Length of y")
})

test_that("print.fregre.np shows bandwidth for NW model", {
  set.seed(42)
  n <- 30
  m <- 20
  X <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)
  fd <- fdars::fdata(X)

  fit <- fdars::fregre.np(fd, y, h = 0.5)
  expect_output(print(fit), "Bandwidth")
})

test_that("print.fregre.np shows k info for kNN models", {
  set.seed(42)
  n <- 30
  m <- 20
  X <- matrix(rnorm(n * m), n, m)
  y <- rnorm(n)
  fd <- fdars::fdata(X)

  # Global CV - single k
  fit_gcv <- fdars::fregre.np(fd, y, type.S = "kNN.gCV", knn = 15)
  expect_output(print(fit_gcv), "Optimal k")

  # Local CV - local k (should print min/max/median)
  fit_lcv <- fdars::fregre.np(fd, y, type.S = "kNN.lCV", knn = 15)
  expect_output(print(fit_lcv), "local")
})
