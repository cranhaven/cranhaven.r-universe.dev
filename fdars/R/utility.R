#' Utility Functions for Functional Data Analysis
#'
#' Various utility functions including integration, inner products,
#' random process generation, and prediction metrics.

# =============================================================================
# Integration
# =============================================================================

#' Simpson's Rule Integration
#'
#' Integrate functional data over its domain using Simpson's rule (composite
#' trapezoidal rule for non-uniform grids). Works with both regular \code{fdata}
#' and irregular \code{irregFdata} objects.
#'
#' @param x A functional data object (\code{fdata} or \code{irregFdata}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of integrals, one per curve.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(2*pi*t)
#' fd <- fdata(X, argvals = t)
#' integrals <- int.simpson(fd)  # Should be approximately 0
#'
#' # Also works with irregular data
#' ifd <- sparsify(fd, minObs = 20, maxObs = 50, seed = 123)
#' integrals_irreg <- int.simpson(ifd)
int.simpson <- function(x, ...) {
  UseMethod("int.simpson")
}

#' @rdname int.simpson
#' @export
int.simpson.fdata <- function(x, ...) {
  if (isTRUE(x$fdata2d)) {
    stop("int.simpson for 2D functional data not yet implemented")
  }

  .Call("wrap__int_simpson", x$data, as.numeric(x$argvals))
}

#' Inner Product of Functional Data
#'
#' Compute the inner product of two functional data objects.
#' <f, g> = integral(f(t) * g(t) dt)
#'
#' @param fdata1 First functional data object.
#' @param fdata2 Second functional data object. If NULL, computes inner
#'   products of fdata1 with itself.
#'
#' @return A matrix of inner products. If fdata1 has n1 curves and fdata2
#'   has n2 curves, returns an n1 x n2 matrix.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' X1 <- matrix(sin(2*pi*t), nrow = 1)
#' X2 <- matrix(cos(2*pi*t), nrow = 1)
#' fd1 <- fdata(X1, argvals = t)
#' fd2 <- fdata(X2, argvals = t)
#' # Inner product of sin and cos over [0,1] should be 0
#' inprod.fdata(fd1, fd2)
inprod.fdata <- function(fdata1, fdata2 = NULL) {
  if (!inherits(fdata1, "fdata")) {
    stop("fdata1 must be of class 'fdata'")
  }

  if (is.null(fdata2)) {
    fdata2 <- fdata1
  } else if (!inherits(fdata2, "fdata")) {
    stop("fdata2 must be of class 'fdata' or NULL")
  }

  if (isTRUE(fdata1$fdata2d) || isTRUE(fdata2$fdata2d)) {
    stop("inprod.fdata for 2D functional data not yet implemented")
  }

  # Check compatible argvals
  if (length(fdata1$argvals) != length(fdata2$argvals)) {
    stop("fdata1 and fdata2 must have the same number of evaluation points")
  }

  .Call("wrap__inprod_fdata", fdata1$data, fdata2$data,
        as.numeric(fdata1$argvals))
}

# =============================================================================
# Random Process Generation
# =============================================================================

#' Generate Ornstein-Uhlenbeck Process
#'
#' Simulate sample paths from an Ornstein-Uhlenbeck process using the
#' Euler-Maruyama discretization scheme.
#'
#' The OU process satisfies the SDE:
#' dX(t) = -theta * X(t) dt + sigma * dW(t)
#'
#' @param n Number of sample paths to generate.
#' @param t Evaluation points (numeric vector).
#' @param mu Long-term mean (default 0).
#' @param theta Mean reversion rate (default 1).
#' @param sigma Volatility (default 1).
#' @param x0 Initial value (default 0).
#' @param seed Optional random seed.
#'
#' @return An fdata object containing the simulated paths.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' ou_data <- r.ou(n = 20, t = t, theta = 2, sigma = 1)
#' plot(ou_data)
r.ou <- function(n, t, mu = 0, theta = 1, sigma = 1, x0 = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  m <- length(t)
  dt <- diff(t)

  # Initialize paths
  X <- matrix(0, n, m)
  X[, 1] <- x0

  # Euler-Maruyama simulation
  for (j in 2:m) {
    dW <- rnorm(n, mean = 0, sd = sqrt(dt[j-1]))
    X[, j] <- X[, j-1] + theta * (mu - X[, j-1]) * dt[j-1] + sigma * dW
  }

  fdata(X, argvals = t, names = list(
    main = "Ornstein-Uhlenbeck Process",
    xlab = "t",
    ylab = "X(t)"
  ))
}

#' Generate Brownian Motion
#'
#' Simulate sample paths from standard Brownian motion (Wiener process).
#'
#' @param n Number of sample paths.
#' @param t Evaluation points.
#' @param sigma Volatility (standard deviation per unit time, default 1).
#' @param x0 Initial value (default 0).
#' @param seed Optional random seed.
#'
#' @return An fdata object containing the simulated paths.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' bm_data <- r.brownian(n = 20, t = t)
#' plot(bm_data)
r.brownian <- function(n, t, sigma = 1, x0 = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  m <- length(t)
  dt <- diff(t)

  # Initialize paths
  X <- matrix(0, n, m)
  X[, 1] <- x0

  # Cumulative sum of increments
  for (j in 2:m) {
    dW <- rnorm(n, mean = 0, sd = sigma * sqrt(dt[j-1]))
    X[, j] <- X[, j-1] + dW
  }

  fdata(X, argvals = t, names = list(
    main = "Brownian Motion",
    xlab = "t",
    ylab = "W(t)"
  ))
}

#' Generate Brownian Bridge
#'
#' Simulate sample paths from a Brownian bridge, which is a Brownian motion
#' conditioned to return to 0 at time 1.
#'
#' @param n Number of sample paths.
#' @param t Evaluation points (should include 0 and 1 for standard bridge).
#' @param sigma Volatility (default 1).
#' @param seed Optional random seed.
#'
#' @return An fdata object containing the simulated paths.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' bb_data <- r.bridge(n = 20, t = t)
#' plot(bb_data)
r.bridge <- function(n, t, sigma = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Standardize to [0, 1]
  t_std <- (t - min(t)) / (max(t) - min(t))
  m <- length(t)

  # Generate Brownian motion
  bm <- r.brownian(n, t, sigma = sigma, seed = NULL)

  # Convert to bridge: B(t) = W(t) - t * W(1)
  W1 <- bm$data[, m]
  X <- bm$data - outer(W1, t_std)

  fdata(X, argvals = t, names = list(
    main = "Brownian Bridge",
    xlab = "t",
    ylab = "B(t)"
  ))
}

# =============================================================================
# Prediction Metrics
# =============================================================================

#' Mean Absolute Error
#'
#' Compute the Mean Absolute Error between predicted and actual values.
#'
#' @param y_true Actual values.
#' @param y_pred Predicted values.
#'
#' @return The mean absolute error.
#'
#' @export
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.9, 4.1, 4.8)
#' pred.MAE(y_true, y_pred)
pred.MAE <- function(y_true, y_pred) {
  mean(abs(y_true - y_pred))
}

#' Mean Squared Error
#'
#' Compute the Mean Squared Error between predicted and actual values.
#'
#' @param y_true Actual values.
#' @param y_pred Predicted values.
#'
#' @return The mean squared error.
#'
#' @export
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.9, 4.1, 4.8)
#' pred.MSE(y_true, y_pred)
pred.MSE <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

#' Root Mean Squared Error
#'
#' Compute the Root Mean Squared Error between predicted and actual values.
#'
#' @param y_true Actual values.
#' @param y_pred Predicted values.
#'
#' @return The root mean squared error.
#'
#' @export
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.9, 4.1, 4.8)
#' pred.RMSE(y_true, y_pred)
pred.RMSE <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2))
}

#' R-Squared (Coefficient of Determination)
#'
#' Compute the R-squared value between predicted and actual values.
#'
#' @param y_true Actual values.
#' @param y_pred Predicted values.
#'
#' @return The R-squared value.
#'
#' @export
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.9, 4.1, 4.8)
#' pred.R2(y_true, y_pred)
pred.R2 <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  1 - ss_res / ss_tot
}
