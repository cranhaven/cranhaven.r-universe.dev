#' Functional Regression
#'
#' Functions for functional regression models.

#' Functional Principal Component Regression
#'
#' Fits a functional linear model using principal component regression.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param ncomp Number of principal components to use.
#' @param ... Additional arguments.
#'
#' @return A fitted regression object of class 'fregre.fd' with components:
#'   \item{coefficients}{Beta coefficient function values}
#'   \item{intercept}{Intercept term}
#'   \item{fitted.values}{Fitted values}
#'   \item{residuals}{Residuals}
#'   \item{ncomp}{Number of components used}
#'   \item{mean.X}{Mean of functional covariate (for prediction)}
#'   \item{mean.y}{Mean of response (for prediction)}
#'   \item{rotation}{PC loadings (for prediction)}
#'   \item{l}{Indices of selected components}
#'   \item{lm}{Underlying linear model}
#'   \item{sr2}{Residual variance}
#'   \item{fdataobj}{Original functional data}
#'   \item{y}{Response vector}
#'   \item{call}{The function call}
#'
#' @export
fregre.pc <- function(fdataobj, y, ncomp = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("fregre.pc not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)
  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Store means for prediction
  X_mean <- colMeans(fdataobj$data)
  y_mean <- mean(y)

  # Center the functional data
  X_centered <- scale(fdataobj$data, center = TRUE, scale = FALSE)
  y_centered <- y - y_mean

  # Compute SVD
  svd_result <- svd(X_centered)

  # Determine number of components
  if (is.null(ncomp)) {
    # Use enough components to explain 95% variance
    var_explained <- cumsum(svd_result$d^2) / sum(svd_result$d^2)
    ncomp <- min(which(var_explained >= 0.95), n - 1)
    ncomp <- max(ncomp, 1)
  }

  ncomp <- min(ncomp, length(svd_result$d))
  l <- seq_len(ncomp)

  # Get PC scores (rotation/loadings)
  rotation <- svd_result$v[, l, drop = FALSE]
  scores <- X_centered %*% rotation
  colnames(scores) <- paste0("PC", l)

  # Fit OLS on scores
  scores_df <- as.data.frame(scores)
  lm_fit <- lm(y_centered ~ ., data = scores_df)

  # Compute beta coefficient function
  beta_coef <- rotation %*% coef(lm_fit)[-1]  # Exclude intercept

  # Compute intercept and fitted values
  intercept <- as.numeric(y_mean - sum(X_mean * beta_coef))
  fitted_values <- as.vector(fdataobj$data %*% beta_coef) + intercept

  # Residual variance
  residuals <- y - fitted_values
  sr2 <- sum(residuals^2) / (n - ncomp - 1)

  structure(
    list(
      coefficients = beta_coef,
      intercept = intercept,
      fitted.values = fitted_values,
      residuals = residuals,
      ncomp = ncomp,
      mean.X = X_mean,
      mean.y = y_mean,
      rotation = rotation,
      l = l,
      lm = lm_fit,
      sr2 = sr2,
      svd = svd_result,
      fdataobj = fdataobj,
      y = y,
      call = match.call()
    ),
    class = "fregre.fd"
  )
}

#' Functional Basis Regression
#'
#' Fits a functional linear model using basis expansion (ridge regression).
#' Uses the anofox-regression Rust backend for efficient L2-regularized regression.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param basis.x Basis for the functional covariate (currently ignored).
#' @param basis.b Basis for the coefficient function (currently ignored).
#' @param lambda Smoothing/regularization parameter (L2 penalty).
#' @param ... Additional arguments.
#'
#' @return A fitted regression object of class 'fregre.fd' with components:
#'   \item{coefficients}{Beta coefficient function values}
#'   \item{intercept}{Intercept term}
#'   \item{fitted.values}{Fitted values}
#'   \item{residuals}{Residuals}
#'   \item{lambda}{Regularization parameter used}
#'   \item{r.squared}{R-squared (coefficient of determination)}
#'   \item{mean.X}{Mean of functional covariate (for prediction)}
#'   \item{mean.y}{Mean of response (for prediction)}
#'   \item{sr2}{Residual variance}
#'   \item{fdataobj}{Original functional data}
#'   \item{y}{Response vector}
#'   \item{call}{The function call}
#'
#' @export
fregre.basis <- function(fdataobj, y, basis.x = NULL, basis.b = NULL,
                         lambda = 0, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Store means for prediction
  X_mean <- colMeans(fdataobj$data)
  y_mean <- mean(y)

  # Note: Rust ridge regression backend is temporarily disabled due to CRAN

  # compatibility constraints (requires faer 0.23+ which needs Rust 1.84+)
  # Always use the pure R implementation for now
  .fregre_basis_r(fdataobj, y, lambda, X_mean, y_mean)
}

# Internal R implementation for underdetermined systems
.fregre_basis_r <- function(fdataobj, y, lambda, X_mean, y_mean) {
  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  # Center the data
  X_centered <- scale(fdataobj$data, center = TRUE, scale = FALSE)
  y_centered <- y - y_mean

  # Ridge regression on centered data: beta = (X'X + lambda*I)^-1 X'y
  XtX <- crossprod(X_centered)
  Xty <- crossprod(X_centered, y_centered)

  if (lambda > 0) {
    XtX <- XtX + lambda * diag(m)
  }

  beta <- solve(XtX, Xty)

  # Compute intercept and fitted values
  intercept <- as.numeric(y_mean - sum(X_mean * beta))
  fitted <- as.vector(fdataobj$data %*% beta) + intercept
  residuals <- y - fitted

  # Compute R-squared
  ss_tot <- sum((y - y_mean)^2)
  ss_res <- sum(residuals^2)
  r_squared <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0

  # Residual variance
  df <- n - m - 1
  if (df <= 0) df <- 1
  sr2 <- sum(residuals^2) / df

  structure(
    list(
      coefficients = beta,
      intercept = intercept,
      fitted.values = fitted,
      residuals = residuals,
      lambda = lambda,
      r.squared = r_squared,
      mean.X = X_mean,
      mean.y = y_mean,
      sr2 = sr2,
      fdataobj = fdataobj,
      y = y,
      call = match.call()
    ),
    class = "fregre.fd"
  )
}

#' Nonparametric Functional Regression
#'
#' Fits a functional regression model using kernel smoothing (Nadaraya-Watson).
#' Supports fixed bandwidth (h), or k-nearest neighbors with global (kNN.gCV)
#' or local (kNN.lCV) cross-validation.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param h Bandwidth parameter. If NULL and knn is NULL, computed automatically.
#' @param knn Number of nearest neighbors to consider for bandwidth selection.
#'   Only used when \code{type.S} is "kNN.gCV" or "kNN.lCV".
#' @param type.S Type of smoother: "S.NW" for Nadaraya-Watson with fixed h (default),
#'   "kNN.gCV" for k-NN with global CV (single k for all observations),
#'   "kNN.lCV" for k-NN with local CV (different k per observation).
#' @param Ker Kernel type for smoothing. Default is "norm" (Gaussian).
#' @param metric Distance metric function. Default is metric.lp.
#' @param ... Additional arguments passed to metric function.
#'
#' @return A fitted regression object of class 'fregre.np' with components:
#' \describe{
#'   \item{fitted.values}{Fitted values}
#'   \item{residuals}{Residuals}
#'   \item{h.opt}{Optimal/used bandwidth (for type.S = "S.NW")}
#'   \item{knn}{Number of neighbors used (for kNN methods)}
#'   \item{k.opt}{Optimal k value(s) - scalar for global, vector for local}
#'   \item{type.S}{Type of smoother used}
#'   \item{Ker}{Kernel type used}
#'   \item{fdataobj}{Original functional data}
#'   \item{y}{Response vector}
#'   \item{mdist}{Distance matrix}
#'   \item{sr2}{Residual variance}
#'   \item{metric}{Metric function used}
#'   \item{call}{The function call}
#' }
#'
#' @details
#' Three smoothing approaches are available:
#'
#' \strong{Fixed bandwidth (type.S = "S.NW")}:
#' Uses a single bandwidth h for all predictions. If h is not provided,
#' it is set to the median of non-zero pairwise distances.
#'
#' \strong{k-NN Global CV (type.S = "kNN.gCV")}:
#' Selects a single optimal k for all observations using leave-one-out
#' cross-validation. The bandwidth at each point is set to include k neighbors.
#'
#' \strong{k-NN Local CV (type.S = "kNN.lCV")}:
#' Selects an optimal k_i for each observation i, allowing adaptive smoothing.
#' Useful when the data has varying density across the functional space.
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 50)
#' n <- 50
#' X <- matrix(0, n, 50)
#' for (i in 1:n) X[i, ] <- sin(2*pi*t) * i/n + rnorm(50, sd = 0.1)
#' y <- rowMeans(X) + rnorm(n, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Fixed bandwidth
#' fit1 <- fregre.np(fd, y, h = 0.5)
#'
#' # k-NN with global CV
#' fit2 <- fregre.np(fd, y, type.S = "kNN.gCV", knn = 20)
#'
#' # k-NN with local CV
#' fit3 <- fregre.np(fd, y, type.S = "kNN.lCV", knn = 20)
fregre.np <- function(fdataobj, y, h = NULL, knn = NULL,
                      type.S = c("S.NW", "kNN.gCV", "kNN.lCV"),
                      Ker = "norm", metric = metric.lp, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  type.S <- match.arg(type.S)
  n <- nrow(fdataobj$data)

  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Compute distance matrix
  D <- as.matrix(metric(fdataobj, ...))

  result <- list(
    fdataobj = fdataobj,
    y = y,
    mdist = D,
    Ker = Ker,
    type.S = type.S,
    metric = metric,
    call = match.call()
  )

  if (type.S == "S.NW") {
    # Fixed bandwidth Nadaraya-Watson
    if (is.null(h)) {
      d_vec <- D[lower.tri(D)]
      h <- median(d_vec[d_vec > 0])
    }

    H <- matrix(0, n, n)
    fitted <- numeric(n)

    for (i in seq_len(n)) {
      weights <- exp(-0.5 * (D[i, ] / h)^2)
      weights[i] <- 0  # Leave-one-out
      if (sum(weights) > 0) {
        weights <- weights / sum(weights)
      }
      H[i, ] <- weights
      fitted[i] <- sum(weights * y)
    }

    result$h.opt <- h
    result$fitted.values <- fitted
    result$H <- H

  } else if (type.S == "kNN.gCV") {
    # k-NN with Global CV
    if (is.null(knn)) knn <- min(20L, n - 2)
    knn <- as.integer(min(knn, n - 2))

    cv_result <- .Call("wrap__knn_gcv", D, as.numeric(y), knn)

    result$knn <- knn
    result$k.opt <- cv_result$k_opt
    result$fitted.values <- cv_result$yhat
    result$mse <- cv_result$mse

  } else if (type.S == "kNN.lCV") {
    # k-NN with Local CV
    if (is.null(knn)) knn <- min(20L, n - 2)
    knn <- as.integer(min(knn, n - 2))

    cv_result <- .Call("wrap__knn_lcv", D, as.numeric(y), knn)

    result$knn <- knn
    result$k.opt <- cv_result$k_opt
    result$fitted.values <- cv_result$yhat
    result$mse <- cv_result$mse
  }

  result$residuals <- y - result$fitted.values

  # Residual variance
  df <- n - 1
  if (!is.null(result$H)) {
    df <- n - sum(diag(result$H))
  }
  if (df <= 0) df <- 1
  result$sr2 <- sum(result$residuals^2) / df

  structure(result, class = "fregre.np")
}

#' Print method for fregre objects
#' @param x A fregre.fd object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fregre.fd <- function(x, ...) {
  cat("Functional regression model\n")
  cat("  Number of observations:", length(x$y), "\n")
  cat("  R-squared:", 1 - sum(x$residuals^2) / sum((x$y - mean(x$y))^2), "\n")
  invisible(x)
}

#' Print method for fregre.np objects
#' @param x A fregre.np object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fregre.np <- function(x, ...) {
  cat("Nonparametric functional regression model\n")
  cat("  Number of observations:", length(x$y), "\n")
  cat("  Smoother type:", x$type.S, "\n")
  if (!is.null(x$h.opt)) {
    cat("  Bandwidth:", x$h.opt, "\n")
  }
  if (!is.null(x$k.opt)) {
    if (length(x$k.opt) == 1) {
      cat("  Optimal k:", x$k.opt, "\n")
    } else {
      cat("  Optimal k (local):", "min =", min(x$k.opt), ", max =", max(x$k.opt),
          ", median =", median(x$k.opt), "\n")
    }
  }
  r2 <- 1 - sum(x$residuals^2) / sum((x$y - mean(x$y))^2)
  cat("  R-squared:", round(r2, 4), "\n")
  invisible(x)
}

#' Cross-Validation for Functional PC Regression
#'
#' Performs k-fold cross-validation to select the optimal number of
#' principal components for functional PC regression.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param kfold Number of folds for cross-validation (default 10).
#' @param ncomp.range Range of number of components to try.
#'   Default is 1 to min(n-1, ncol(data)).
#' @param seed Random seed for fold assignment.
#' @param ... Additional arguments passed to fregre.pc.
#'
#' @return A list with components:
#' \describe{
#'   \item{optimal.ncomp}{Optimal number of components}
#'   \item{cv.errors}{Mean squared prediction error for each ncomp}
#'   \item{cv.se}{Standard error of cv.errors}
#'   \item{model}{Fitted model with optimal ncomp}
#' }
#'
#' @export
#' @examples
#' # Create functional data with a linear relationship
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 100, 50)
#' for (i in 1:100) X[i, ] <- sin(2*pi*t) * i/100 + rnorm(50, sd = 0.1)
#' beta_true <- cos(2*pi*t)
#' y <- X %*% beta_true + rnorm(100, sd = 0.5)
#' fd <- fdata(X, argvals = t)
#'
#' # Cross-validate to find optimal number of PCs
#' cv_result <- fregre.pc.cv(fd, y, ncomp.range = 1:10)
fregre.pc.cv <- function(fdataobj, y, kfold = 10, ncomp.range = NULL,
                         seed = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Set default ncomp.range
  if (is.null(ncomp.range)) {
    max_comp <- min(n - 1, m)
    ncomp.range <- seq_len(min(max_comp, 15))
  }
  ncomp.range <- ncomp.range[ncomp.range < n & ncomp.range <= m]

  if (length(ncomp.range) == 0) {
    stop("No valid values in ncomp.range")
  }

  # Create fold assignments
  if (!is.null(seed)) set.seed(seed)
  folds <- sample(rep(seq_len(kfold), length.out = n))

  # Initialize storage for CV errors
  cv_errors <- matrix(0, nrow = length(ncomp.range), ncol = kfold)

  for (k in seq_len(kfold)) {
    # Split data
    test_idx <- which(folds == k)
    train_idx <- which(folds != k)

    # Create train/test fdata objects
    fd_train <- fdataobj[train_idx, ]
    fd_test <- fdataobj[test_idx, ]
    y_train <- y[train_idx]
    y_test <- y[test_idx]

    for (j in seq_along(ncomp.range)) {
      ncomp <- ncomp.range[j]

      # Fit model on training data
      tryCatch({
        fit <- fregre.pc(fd_train, y_train, ncomp = ncomp, ...)

        # Predict on test data
        y_pred <- as.vector(fd_test$data %*% fit$coefficients) + fit$intercept

        # Compute MSE
        cv_errors[j, k] <- mean((y_test - y_pred)^2)
      }, error = function(e) {
        cv_errors[j, k] <<- NA
      })
    }
  }

  # Compute mean and SE of CV errors
  mean_cv <- rowMeans(cv_errors, na.rm = TRUE)
  se_cv <- apply(cv_errors, 1, sd, na.rm = TRUE) / sqrt(kfold)
  names(mean_cv) <- names(se_cv) <- ncomp.range

  # Find optimal ncomp (minimum CV error)
  optimal_idx <- which.min(mean_cv)
  optimal_ncomp <- ncomp.range[optimal_idx]

  # Fit final model with optimal ncomp
  final_model <- fregre.pc(fdataobj, y, ncomp = optimal_ncomp, ...)

  list(
    optimal.ncomp = optimal_ncomp,
    cv.errors = mean_cv,
    cv.se = se_cv,
    model = final_model
  )
}

#' Cross-Validation for Functional Basis Regression
#'
#' Performs k-fold cross-validation to select the optimal regularization
#' parameter (lambda) for functional basis regression.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param kfold Number of folds for cross-validation (default 10).
#' @param lambda.range Range of lambda values to try.
#'   Default is 10^seq(-4, 4, length.out = 20).
#' @param seed Random seed for fold assignment.
#' @param ... Additional arguments passed to fregre.basis.
#'
#' @return A list with components:
#' \describe{
#'   \item{optimal.lambda}{Optimal regularization parameter}
#'   \item{cv.errors}{Mean squared prediction error for each lambda}
#'   \item{cv.se}{Standard error of cv.errors}
#'   \item{model}{Fitted model with optimal lambda}
#' }
#'
#' @export
fregre.basis.cv <- function(fdataobj, y, kfold = 10, lambda.range = NULL,
                            seed = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)

  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Set default lambda.range
  if (is.null(lambda.range)) {
    lambda.range <- 10^seq(-4, 4, length.out = 20)
  }

  # Create fold assignments
  if (!is.null(seed)) set.seed(seed)
  folds <- sample(rep(seq_len(kfold), length.out = n))

  # Initialize storage for CV errors
  cv_errors <- matrix(0, nrow = length(lambda.range), ncol = kfold)

  for (k in seq_len(kfold)) {
    # Split data
    test_idx <- which(folds == k)
    train_idx <- which(folds != k)

    # Create train/test fdata objects
    fd_train <- fdataobj[train_idx, ]
    fd_test <- fdataobj[test_idx, ]
    y_train <- y[train_idx]
    y_test <- y[test_idx]

    for (j in seq_along(lambda.range)) {
      lambda <- lambda.range[j]

      # Fit model on training data
      tryCatch({
        fit <- fregre.basis(fd_train, y_train, lambda = lambda, ...)

        # Predict on test data
        y_pred <- as.vector(fd_test$data %*% fit$coefficients)

        # Compute MSE
        cv_errors[j, k] <- mean((y_test - y_pred)^2)
      }, error = function(e) {
        cv_errors[j, k] <<- NA
      })
    }
  }

  # Compute mean and SE of CV errors
  mean_cv <- rowMeans(cv_errors, na.rm = TRUE)
  se_cv <- apply(cv_errors, 1, sd, na.rm = TRUE) / sqrt(kfold)
  names(mean_cv) <- names(se_cv) <- lambda.range

  # Find optimal lambda (minimum CV error)
  optimal_idx <- which.min(mean_cv)
  optimal_lambda <- lambda.range[optimal_idx]

  # Fit final model with optimal lambda
  final_model <- fregre.basis(fdataobj, y, lambda = optimal_lambda, ...)

  list(
    optimal.lambda = optimal_lambda,
    cv.errors = mean_cv,
    cv.se = se_cv,
    model = final_model
  )
}

#' Cross-Validation for Nonparametric Functional Regression
#'
#' Performs k-fold cross-validation to select the optimal bandwidth
#' parameter (h) for nonparametric functional regression.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param kfold Number of folds for cross-validation (default 10).
#' @param h.range Range of bandwidth values to try. If NULL, automatically
#'   determined from the distance matrix.
#' @param metric Distance metric function. Default is metric.lp.
#' @param seed Random seed for fold assignment.
#' @param ... Additional arguments passed to the metric function.
#'
#' @return A list with components:
#' \describe{
#'   \item{optimal.h}{Optimal bandwidth parameter}
#'   \item{cv.errors}{Mean squared prediction error for each h}
#'   \item{cv.se}{Standard error of cv.errors}
#'   \item{model}{Fitted model with optimal h}
#' }
#'
#' @export
fregre.np.cv <- function(fdataobj, y, kfold = 10, h.range = NULL,
                         metric = metric.lp, seed = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)

  if (length(y) != n) {
    stop("Length of y must equal number of curves in fdataobj")
  }

  # Compute full distance matrix once
  D <- metric(fdataobj, ...)

  # Set default h.range based on distances
  if (is.null(h.range)) {
    d_vec <- D[lower.tri(D)]
    d_vec <- d_vec[d_vec > 0]
    h.range <- quantile(d_vec, probs = seq(0.05, 0.95, length.out = 20))
  }

  # Create fold assignments
  if (!is.null(seed)) set.seed(seed)
  folds <- sample(rep(seq_len(kfold), length.out = n))

  # Initialize storage for CV errors
  cv_errors <- matrix(0, nrow = length(h.range), ncol = kfold)

  for (k in seq_len(kfold)) {
    # Split data
    test_idx <- which(folds == k)
    train_idx <- which(folds != k)

    y_train <- y[train_idx]
    y_test <- y[test_idx]

    # Extract sub-distance matrices
    D_train <- D[train_idx, train_idx]
    D_test_train <- D[test_idx, train_idx, drop = FALSE]

    for (j in seq_along(h.range)) {
      h <- h.range[j]

      tryCatch({
        # Nadaraya-Watson prediction for test set
        n_test <- length(test_idx)
        y_pred <- numeric(n_test)

        for (i in seq_len(n_test)) {
          # Gaussian kernel weights
          weights <- exp(-0.5 * (D_test_train[i, ] / h)^2)
          weights <- weights / sum(weights)
          y_pred[i] <- sum(weights * y_train)
        }

        # Compute MSE
        cv_errors[j, k] <- mean((y_test - y_pred)^2)
      }, error = function(e) {
        cv_errors[j, k] <<- NA
      })
    }
  }

  # Compute mean and SE of CV errors
  mean_cv <- rowMeans(cv_errors, na.rm = TRUE)
  se_cv <- apply(cv_errors, 1, sd, na.rm = TRUE) / sqrt(kfold)
  names(mean_cv) <- names(se_cv) <- h.range

  # Find optimal h (minimum CV error)
  optimal_idx <- which.min(mean_cv)
  optimal_h <- h.range[optimal_idx]

  # Fit final model with optimal h
  final_model <- fregre.np(fdataobj, y, h = optimal_h, metric = metric, ...)

  list(
    optimal.h = optimal_h,
    cv.errors = mean_cv,
    cv.se = se_cv,
    model = final_model
  )
}

# =============================================================================
# Predict Methods
# =============================================================================

#' Predict Method for Functional Regression (fregre.fd)
#'
#' Predictions from a fitted functional regression model (fregre.pc or fregre.basis).
#'
#' @param object A fitted model object of class 'fregre.fd'.
#' @param newdata An fdata object containing new functional data for prediction.
#'   If NULL, returns fitted values from training data.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of predicted values.
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:30) X[i, ] <- sin(2*pi*t) * i/30 + rnorm(50, sd = 0.1)
#' y <- rowMeans(X) + rnorm(30, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Fit model
#' fit <- fregre.pc(fd, y, ncomp = 3)
#'
#' # Predict on new data
#' X_new <- matrix(sin(2*pi*t) * 0.5, nrow = 1)
#' fd_new <- fdata(X_new, argvals = t)
#' predict(fit, fd_new)
predict.fregre.fd <- function(object, newdata = NULL, ...) {
  if (is.null(object)) {
    stop("No fregre.fd object provided")
  }

  # If no new data, return fitted values
  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  # Convert to fdata if needed
  if (!inherits(newdata, "fdata")) {
    newdata <- fdata(newdata,
                     argvals = object$fdataobj$argvals,
                     rangeval = object$fdataobj$rangeval)
  }

  # Get new data matrix
  X_new <- newdata$data
  nn <- nrow(X_new)

  # Check dimensions match
  if (ncol(X_new) != ncol(object$fdataobj$data)) {
    stop("Number of evaluation points in newdata must match training data")
  }

  # Compute predictions using stored coefficients and intercept
  if (!is.null(object$intercept)) {
    y_pred <- as.vector(X_new %*% object$coefficients) + object$intercept
  } else {
    y_pred <- as.vector(X_new %*% object$coefficients)
  }

  names(y_pred) <- rownames(X_new)
  y_pred
}

#' Predict Method for Nonparametric Functional Regression (fregre.np)
#'
#' Predictions from a fitted nonparametric functional regression model.
#'
#' @param object A fitted model object of class 'fregre.np'.
#' @param newdata An fdata object containing new functional data for prediction.
#'   If NULL, returns fitted values from training data.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of predicted values.
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:30) X[i, ] <- sin(2*pi*t) * i/30 + rnorm(50, sd = 0.1)
#' y <- rowMeans(X) + rnorm(30, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Fit model
#' fit <- fregre.np(fd, y)
#'
#' # Predict on new data
#' X_new <- matrix(sin(2*pi*t) * 0.5, nrow = 1)
#' fd_new <- fdata(X_new, argvals = t)
#' predict(fit, fd_new)
predict.fregre.np <- function(object, newdata = NULL, ...) {
  if (is.null(object)) {
    stop("No fregre.np object provided")
  }

  # If no new data, return fitted values
  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  # Convert to fdata if needed
  if (!inherits(newdata, "fdata")) {
    newdata <- fdata(newdata,
                     argvals = object$fdataobj$argvals,
                     rangeval = object$fdataobj$rangeval)
  }

  # Check dimensions match
  if (ncol(newdata$data) != ncol(object$fdataobj$data)) {
    stop("Number of evaluation points in newdata must match training data")
  }

  nn <- nrow(newdata$data)
  n_train <- nrow(object$fdataobj$data)
  y_train <- object$y

  # Compute distances from new data to training data
  metric_fn <- object$metric
  D_new <- as.matrix(metric_fn(object$fdataobj, newdata))

  type.S <- if (!is.null(object$type.S)) object$type.S else "S.NW"

  if (type.S == "S.NW") {
    # Fixed bandwidth Nadaraya-Watson prediction
    h <- object$h.opt
    y_pred <- numeric(nn)

    for (i in seq_len(nn)) {
      weights <- exp(-0.5 * (D_new[, i] / h)^2)
      if (sum(weights) > 0) {
        weights <- weights / sum(weights)
      } else {
        weights <- rep(1/n_train, n_train)
      }
      y_pred[i] <- sum(weights * y_train)
    }

  } else if (type.S %in% c("kNN.gCV", "kNN.lCV")) {
    # k-NN prediction using Rust backend
    k_opt <- object$k.opt

    if (length(k_opt) == 1) {
      # Global k - pass as single integer
      y_pred <- .Call("wrap__knn_predict", D_new, as.numeric(y_train),
                      as.integer(k_opt), NULL)
    } else {
      # Local k - pass as vector
      y_pred <- .Call("wrap__knn_predict", D_new, as.numeric(y_train),
                      0L, as.integer(k_opt))
    }
  }

  names(y_pred) <- rownames(newdata$data)
  y_pred
}

#' Nonparametric Regression with Multiple Functional Predictors
#'
#' Fits a nonparametric regression model with multiple functional predictors
#' using a weighted combination of distance matrices.
#'
#' @param fdataobj.list A list of fdata objects (functional predictors).
#'   All must have the same number of observations.
#' @param y Response vector (scalar).
#' @param weights Weights for combining distances. Can be:
#'   \itemize{
#'     \item NULL: Equal weights (1/p for each predictor)
#'     \item Numeric vector: Fixed weights (will be normalized to sum to 1)
#'     \item "cv": Cross-validate to find optimal weights
#'   }
#' @param h Bandwidth for Nadaraya-Watson kernel (optional).
#' @param knn Maximum k for k-NN methods.
#' @param type.S Smoother type: "S.NW", "kNN.gCV", or "kNN.lCV".
#' @param Ker Kernel type (default "norm" for Gaussian).
#' @param metric Distance metric function (default metric.lp).
#' @param cv.grid Grid of weight values for CV (only used if weights = "cv").
#'   Default is seq(0, 1, by = 0.1) for 2 predictors.
#' @param cv.folds Number of folds for weight CV (default 5).
#' @param ... Additional arguments passed to metric function.
#'
#' @return An object of class 'fregre.np.multi' containing:
#' \describe{
#'   \item{fdataobj.list}{List of functional predictors}
#'   \item{y}{Response vector}
#'   \item{weights}{Weights used (or optimized)}
#'   \item{weights.cv}{CV results if weights = "cv"}
#'   \item{fitted.values}{Fitted values}
#'   \item{residuals}{Residuals}
#'   \item{D.list}{List of distance matrices}
#'   \item{D.combined}{Combined distance matrix}
#' }
#'
#' @export
#' @examples
#' # Create two functional predictors
#' set.seed(42)
#' n <- 50
#' m <- 30
#' t_grid <- seq(0, 1, length.out = m)
#'
#' X1 <- matrix(0, n, m)
#' X2 <- matrix(0, n, m)
#' for (i in 1:n) {
#'   X1[i, ] <- sin(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
#'   X2[i, ] <- cos(2 * pi * t_grid) * i/n + rnorm(m, sd = 0.1)
#' }
#' y <- rowMeans(X1) + 0.5 * rowMeans(X2) + rnorm(n, sd = 0.1)
#'
#' fd1 <- fdata(X1, argvals = t_grid)
#' fd2 <- fdata(X2, argvals = t_grid)
#'
#' # Fit with equal weights
#' fit1 <- fregre.np.multi(list(fd1, fd2), y)
#'
#' # Fit with cross-validated weights
#' fit2 <- fregre.np.multi(list(fd1, fd2), y, weights = "cv")
fregre.np.multi <- function(fdataobj.list, y, weights = NULL,
                            h = NULL, knn = NULL,
                            type.S = c("S.NW", "kNN.gCV", "kNN.lCV"),
                            Ker = "norm", metric = metric.lp,
                            cv.grid = NULL, cv.folds = 5, ...) {

  # Validate inputs
  if (!is.list(fdataobj.list)) {
    stop("fdataobj.list must be a list of fdata objects")
  }

  p <- length(fdataobj.list)
  if (p < 1) {
    stop("Need at least one functional predictor")
  }

  # Check all are fdata with same n
  n_vec <- sapply(fdataobj.list, function(fd) {
    if (!inherits(fd, "fdata")) {
      stop("All elements of fdataobj.list must be fdata objects")
    }
    nrow(fd$data)
  })

  if (length(unique(n_vec)) > 1) {
    stop("All functional predictors must have the same number of observations")
  }

  n <- n_vec[1]
  type.S <- match.arg(type.S)

  if (length(y) != n) {
    stop("Length of y must equal number of observations (", n, ")")
  }

  # Compute distance matrices for each predictor
  D.list <- lapply(fdataobj.list, function(fd) {
    as.matrix(metric(fd, ...))
  })

  # Normalize each distance matrix to [0, 1] range for fair comparison
  D.list.norm <- lapply(D.list, function(D) {
    max_d <- max(D)
    if (max_d > 0) D / max_d else D
  })

  # Handle weights
  cv_result <- NULL

  if (is.null(weights)) {
    # Equal weights
    weights <- rep(1 / p, p)

  } else if (is.character(weights) && weights == "cv") {
    # Cross-validate weights
    cv_result <- .cv_weights_multi(D.list.norm, y, type.S, h, knn, Ker,
                                   cv.grid, cv.folds, p)
    weights <- cv_result$optimal.weights

  } else if (is.numeric(weights)) {
    if (length(weights) != p) {
      stop("Length of weights must equal number of predictors (", p, ")")
    }
    # Normalize to sum to 1
    weights <- weights / sum(weights)
  }

  # Combine distance matrices
  D.combined <- matrix(0, n, n)
  for (i in seq_len(p)) {
    D.combined <- D.combined + weights[i] * D.list.norm[[i]]
  }

  # Fit using combined distance
  result <- .fit_np_with_distance(D.combined, y, h, knn, type.S, Ker)

  # Build result object
  result$fdataobj.list <- fdataobj.list
  result$y <- y
  result$weights <- weights
  result$weights.cv <- cv_result
  result$D.list <- D.list
  result$D.combined <- D.combined
  result$metric <- metric
  result$type.S <- type.S
  result$Ker <- Ker
  result$call <- match.call()

  class(result) <- "fregre.np.multi"
  result
}

# Internal: Cross-validate weights for multiple predictors
.cv_weights_multi <- function(D.list.norm, y, type.S, h, knn, Ker,
                              cv.grid, cv.folds, p) {
  n <- length(y)

  # Generate weight grid
  if (is.null(cv.grid)) {
    if (p == 2) {
      # For 2 predictors, simple 1D grid
      w1_grid <- seq(0, 1, by = 0.1)
      weight_grid <- lapply(w1_grid, function(w1) c(w1, 1 - w1))
    } else {
      # For >2 predictors, use deterministic simplex grid
      # Generate evenly spaced points using Halton-like sequence
      n_grid <- 20
      primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)[seq_len(min(p, 10))]
      weight_grid <- lapply(seq_len(n_grid), function(i) {
        w <- numeric(p)
        for (j in seq_len(p)) {
          # Van der Corput sequence for dimension j
          base <- primes[((j - 1) %% length(primes)) + 1]
          val <- 0; f <- 1; ii <- i
          while (ii > 0) { f <- f / base; val <- val + (ii %% base) * f; ii <- ii %/% base }
          w[j] <- val + 0.01
        }
        w / sum(w)
      })
      # Add equal weights and extreme weights
      weight_grid <- c(weight_grid, list(rep(1/p, p)))
      for (i in seq_len(p)) {
        extreme <- rep(0, p)
        extreme[i] <- 1
        weight_grid <- c(weight_grid, list(extreme))
      }
    }
  } else {
    weight_grid <- cv.grid
  }

  # Create folds
  fold_ids <- sample(rep(seq_len(cv.folds), length.out = n))

  # Evaluate each weight combination
  cv_errors <- sapply(weight_grid, function(w) {
    # Combine distances
    D.combined <- matrix(0, n, n)
    for (i in seq_len(p)) {
      D.combined <- D.combined + w[i] * D.list.norm[[i]]
    }

    # K-fold CV
    fold_errors <- sapply(seq_len(cv.folds), function(fold) {
      test_idx <- which(fold_ids == fold)
      train_idx <- which(fold_ids != fold)

      # Fit on training, predict on test
      D_train <- D.combined[train_idx, train_idx]
      y_train <- y[train_idx]
      D_test <- D.combined[test_idx, train_idx]

      # Simple NW prediction for CV
      if (is.null(h)) {
        d_vec <- D_train[lower.tri(D_train)]
        h_cv <- median(d_vec[d_vec > 0])
      } else {
        h_cv <- h
      }

      y_pred <- sapply(seq_len(nrow(D_test)), function(i) {
        weights_nw <- exp(-0.5 * (D_test[i, ] / h_cv)^2)
        if (sum(weights_nw) > 0) {
          sum(weights_nw * y_train) / sum(weights_nw)
        } else {
          mean(y_train)
        }
      })

      mean((y[test_idx] - y_pred)^2)
    })

    mean(fold_errors)
  })

  # Find best weights
  best_idx <- which.min(cv_errors)
  optimal_weights <- weight_grid[[best_idx]]

  list(
    optimal.weights = optimal_weights,
    cv.errors = cv_errors,
    weight.grid = weight_grid,
    best.idx = best_idx
  )
}

# Internal: Fit NP regression with pre-computed distance matrix
.fit_np_with_distance <- function(D, y, h, knn, type.S, Ker) {
  n <- length(y)

  result <- list()

  if (type.S == "S.NW") {
    # Fixed bandwidth Nadaraya-Watson
    if (is.null(h)) {
      d_vec <- D[lower.tri(D)]
      h <- median(d_vec[d_vec > 0])
    }

    H <- matrix(0, n, n)
    fitted <- numeric(n)

    for (i in seq_len(n)) {
      weights <- exp(-0.5 * (D[i, ] / h)^2)
      weights[i] <- 0  # Leave-one-out
      if (sum(weights) > 0) {
        weights <- weights / sum(weights)
      }
      H[i, ] <- weights
      fitted[i] <- sum(weights * y)
    }

    result$h.opt <- h
    result$fitted.values <- fitted
    result$H <- H

  } else if (type.S == "kNN.gCV") {
    # k-NN with Global CV
    if (is.null(knn)) knn <- min(20L, n - 2)
    knn <- as.integer(min(knn, n - 2))

    cv_result <- .Call("wrap__knn_gcv", D, as.numeric(y), knn)

    result$knn <- knn
    result$k.opt <- cv_result$k_opt
    result$fitted.values <- cv_result$yhat
    result$mse <- cv_result$mse

  } else if (type.S == "kNN.lCV") {
    # k-NN with Local CV
    if (is.null(knn)) knn <- min(20L, n - 2)
    knn <- as.integer(min(knn, n - 2))

    cv_result <- .Call("wrap__knn_lcv", D, as.numeric(y), knn)

    result$knn <- knn
    result$k.opt <- cv_result$k_opt
    result$fitted.values <- cv_result$yhat
    result$mse <- cv_result$mse
  }

  result$residuals <- y - result$fitted.values

  # Residual variance
  df <- n - 1
  if (!is.null(result$H)) {
    df <- n - sum(diag(result$H))
  }
  if (df <= 0) df <- 1
  result$sr2 <- sum(result$residuals^2) / df

  result
}

#' Print method for fregre.np.multi
#' @param x A fregre.np.multi object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fregre.np.multi <- function(x, ...) {
  cat("Nonparametric functional regression with multiple predictors\n")
  cat("=============================================================\n")
  cat("Number of observations:", length(x$y), "\n")
  cat("Number of functional predictors:", length(x$fdataobj.list), "\n")
  cat("Smoother type:", x$type.S, "\n")
  cat("Weights:", paste(round(x$weights, 3), collapse = ", "), "\n")

  if (!is.null(x$weights.cv)) {
    cat("  (cross-validated)\n")
  }

  if (!is.null(x$h.opt)) {
    cat("Bandwidth:", round(x$h.opt, 4), "\n")
  }
  if (!is.null(x$k.opt)) {
    if (length(x$k.opt) == 1) {
      cat("Optimal k:", x$k.opt, "\n")
    } else {
      cat("Optimal k (local): min =", min(x$k.opt), ", max =", max(x$k.opt), "\n")
    }
  }

  r2 <- 1 - sum(x$residuals^2) / sum((x$y - mean(x$y))^2)
  cat("R-squared:", round(r2, 4), "\n")

  invisible(x)
}

#' Predict method for fregre.np.multi
#'
#' @param object Fitted fregre.np.multi object.
#' @param newdata.list List of new fdata objects (same length as original).
#' @param ... Additional arguments.
#'
#' @return Predicted values.
#' @export
predict.fregre.np.multi <- function(object, newdata.list = NULL, ...) {
  if (is.null(newdata.list)) {
    return(object$fitted.values)
  }

  p <- length(object$fdataobj.list)
  if (length(newdata.list) != p) {
    stop("newdata.list must have same length as original fdataobj.list (", p, ")")
  }

  # Compute distances from new data to training data
  n_new <- nrow(newdata.list[[1]]$data)
  n_train <- nrow(object$fdataobj.list[[1]]$data)

  D_new_list <- lapply(seq_len(p), function(i) {
    # Combine new and training data
    combined <- fdata(
      rbind(newdata.list[[i]]$data, object$fdataobj.list[[i]]$data),
      argvals = object$fdataobj.list[[i]]$argvals
    )
    D_full <- as.matrix(object$metric(combined, ...))

    # Extract new-to-train distances and normalize
    D_sub <- D_full[seq_len(n_new), (n_new + 1):(n_new + n_train), drop = FALSE]

    # Normalize using training max
    max_train <- max(object$D.list[[i]])
    if (max_train > 0) D_sub / max_train else D_sub
  })

  # Combine with weights
  D_new <- matrix(0, n_new, n_train)
  for (i in seq_len(p)) {
    D_new <- D_new + object$weights[i] * D_new_list[[i]]
  }

  y_train <- object$y
  type.S <- object$type.S

  if (type.S == "S.NW") {
    h <- object$h.opt
    y_pred <- sapply(seq_len(n_new), function(i) {
      weights <- exp(-0.5 * (D_new[i, ] / h)^2)
      if (sum(weights) > 0) {
        sum(weights * y_train) / sum(weights)
      } else {
        mean(y_train)
      }
    })

  } else if (type.S %in% c("kNN.gCV", "kNN.lCV")) {
    k_opt <- object$k.opt

    if (length(k_opt) == 1) {
      y_pred <- .Call("wrap__knn_predict", D_new, as.numeric(y_train),
                      as.integer(k_opt), NULL)
    } else {
      y_pred <- .Call("wrap__knn_predict", D_new, as.numeric(y_train),
                      0L, as.integer(k_opt))
    }
  }

  y_pred
}
