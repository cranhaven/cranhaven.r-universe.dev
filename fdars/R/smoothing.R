#' Smoothing Functions for Functional Data
#'
#' Functions for computing smoothing matrices and applying kernel smoothing
#' to functional data.

# =============================================================================
# Smoothing Matrix Functions
# =============================================================================

#' Nadaraya-Watson Kernel Smoother Matrix
#'
#' Compute the Nadaraya-Watson kernel smoother matrix.
#'
#' @param tt Evaluation points (numeric vector).
#' @param h Bandwidth parameter.
#' @param Ker Kernel function or name. One of "norm", "epa", "tri", "quar",
#'   "cos", "unif", or a custom function.
#' @param w Optional weights vector of length n.
#' @param cv Logical. If TRUE, compute leave-one-out cross-validation matrix
#'   (diagonal is zero).
#'
#' @return An n x n smoother matrix S such that smooth(y) = S %*% y.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' S <- S.NW(tt, h = 0.1)
#' dim(S)  # 50 x 50
S.NW <- function(tt, h, Ker = "norm", w = NULL, cv = FALSE) {
  if (is.null(w)) w <- rep(1, length(tt))

  # Handle kernel specification
  kernel_type <- if (is.function(Ker)) {
    # Custom kernel not directly supported, use normal
    warning("Custom kernel functions not yet supported in Rust backend, using normal kernel")
    "norm"
  } else if (is.character(Ker)) {
    Ker
  } else {
    "norm"
  }

  .Call("wrap__s_nw", as.numeric(tt), as.numeric(h), kernel_type,
        as.numeric(w), as.logical(cv))
}

#' Local Linear Regression Smoother Matrix
#'
#' Compute the Local Linear Regression (LLR) smoother matrix.
#' LLR has better boundary bias properties than Nadaraya-Watson.
#'
#' @param tt Evaluation points (numeric vector).
#' @param h Bandwidth parameter.
#' @param Ker Kernel function or name. One of "norm", "epa", "tri", "quar",
#'   "cos", "unif".
#' @param w Optional weights vector of length n.
#' @param cv Logical. If TRUE, compute leave-one-out cross-validation matrix.
#'
#' @return An n x n smoother matrix S.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' S <- S.LLR(tt, h = 0.1)
S.LLR <- function(tt, h, Ker = "norm", w = NULL, cv = FALSE) {
  if (is.null(w)) w <- rep(1, length(tt))

  kernel_type <- if (is.character(Ker)) Ker else "norm"

  .Call("wrap__s_llr", as.numeric(tt), as.numeric(h), kernel_type,
        as.numeric(w), as.logical(cv))
}

#' Local Polynomial Regression Smoother Matrix
#'
#' Compute the Local Polynomial Regression smoother matrix of degree p.
#' Special cases: p=0 is Nadaraya-Watson, p=1 is Local Linear Regression.
#'
#' @param tt Evaluation points (numeric vector).
#' @param h Bandwidth parameter.
#' @param p Polynomial degree (default 1 for local linear).
#' @param Ker Kernel function or name.
#' @param w Optional weights vector.
#' @param cv Logical. If TRUE, compute leave-one-out cross-validation matrix.
#'
#' @return An n x n smoother matrix S.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' S <- S.LPR(tt, h = 0.1, p = 2)  # Local quadratic regression
S.LPR <- function(tt, h, p = 1, Ker = "norm", w = NULL, cv = FALSE) {
  if (is.null(w)) w <- rep(1, length(tt))

  kernel_type <- if (is.character(Ker)) Ker else "norm"

  .Call("wrap__s_lpr", as.numeric(tt), as.numeric(h), as.integer(p),
        kernel_type, as.numeric(w), as.logical(cv))
}

#' Local Cubic Regression Smoother Matrix
#'
#' Convenience function for Local Polynomial Regression with degree 3.
#'
#' @param tt Evaluation points (numeric vector).
#' @param h Bandwidth parameter.
#' @param Ker Kernel function or name.
#' @param w Optional weights vector.
#' @param cv Logical. If TRUE, compute leave-one-out cross-validation matrix.
#'
#' @return An n x n smoother matrix S.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' S <- S.LCR(tt, h = 0.15)
S.LCR <- function(tt, h, Ker = "norm", w = NULL, cv = FALSE) {
  S.LPR(tt, h, p = 3, Ker = Ker, w = w, cv = cv)
}

#' K-Nearest Neighbors Smoother Matrix
#'
#' Compute a smoother matrix using adaptive bandwidth based on the k nearest
#' neighbors. The bandwidth at each point is the distance to the k-th nearest
#' neighbor.
#'
#' @param tt Evaluation points (numeric vector).
#' @param knn Number of nearest neighbors.
#' @param Ker Kernel function or name.
#' @param w Optional weights vector.
#' @param cv Logical. If TRUE, compute leave-one-out cross-validation matrix.
#'
#' @return An n x n smoother matrix S.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' S <- S.KNN(tt, knn = 10)
S.KNN <- function(tt, knn, Ker = "norm", w = NULL, cv = FALSE) {
  if (is.null(w)) w <- rep(1, length(tt))

  kernel_type <- if (is.character(Ker)) Ker else "norm"

  .Call("wrap__s_knn", as.numeric(tt), as.integer(knn), kernel_type,
        as.numeric(w), as.logical(cv))
}

# =============================================================================
# Bandwidth Selection
# =============================================================================

#' Default Bandwidth
#'
#' Compute a default bandwidth as the 15th percentile of pairwise distances.
#'
#' @param fdataobj An object of class 'fdata', or a numeric vector of
#'   evaluation points.
#' @param ... Additional arguments (ignored).
#'
#' @return A scalar bandwidth value.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' h <- h.default(tt)
h.default <- function(fdataobj, ...) {
  if (inherits(fdataobj, "fdata")) {
    tt <- fdataobj$argvals
  } else {
    tt <- as.numeric(fdataobj)
  }

  # Compute pairwise distances
  n <- length(tt)
  dists <- abs(outer(tt, tt, "-"))
  dists <- dists[lower.tri(dists)]

  # Return 15th percentile
  quantile(dists, 0.15)
}

#' Cross-Validation for Smoother Selection
#'
#' Compute leave-one-out cross-validation criterion for a smoother.
#'
#' @param S.type Function to compute smoother matrix (e.g., S.NW, S.LLR).
#' @param tt Evaluation points.
#' @param h Bandwidth parameter.
#' @param y Response vector to smooth.
#' @param Ker Kernel type.
#' @param w Optional weights.
#'
#' @return The cross-validation score (mean squared prediction error).
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' y <- sin(2 * pi * tt) + rnorm(50, sd = 0.1)
#' cv_score <- CV.S(S.NW, tt, h = 0.1, y = y)
CV.S <- function(S.type, tt, h, y, Ker = "norm", w = NULL) {
  # Get LOO smoother matrix
  S_cv <- S.type(tt, h = h, Ker = Ker, w = w, cv = TRUE)

  # Compute LOO predictions
  y_hat <- as.vector(S_cv %*% y)

  # CV score: mean squared error
  mean((y - y_hat)^2)
}

#' Generalized Cross-Validation for Smoother Selection
#'
#' Compute GCV criterion: RSS / (1 - tr(S)/n)^2
#'
#' @param S.type Function to compute smoother matrix.
#' @param tt Evaluation points.
#' @param h Bandwidth parameter.
#' @param y Response vector.
#' @param Ker Kernel type.
#' @param w Optional weights.
#'
#' @return The GCV score.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' y <- sin(2 * pi * tt) + rnorm(50, sd = 0.1)
#' gcv_score <- GCV.S(S.NW, tt, h = 0.1, y = y)
GCV.S <- function(S.type, tt, h, y, Ker = "norm", w = NULL) {
  n <- length(y)

  # Get smoother matrix (not CV version)
  S <- S.type(tt, h = h, Ker = Ker, w = w, cv = FALSE)

  # Fitted values and residuals
  y_hat <- as.vector(S %*% y)
  residuals <- y - y_hat

  # RSS and trace of S
  rss <- sum(residuals^2)
  trace_s <- sum(diag(S))

  # GCV = RSS / (1 - trace(S)/n)^2
  denom <- (1 - trace_s / n)^2
  if (denom < 1e-10) {
    return(Inf)
  }

  rss / n / denom
}

#' Optimize Bandwidth Using Cross-Validation
#'
#' Find the optimal bandwidth by minimizing CV or GCV criterion.
#'
#' @param fdataobj An fdata object.
#' @param S.type Smoother function (S.NW, S.LLR, etc.).
#' @param h.range Range of bandwidths to search (default: data-driven).
#' @param criterion "CV" or "GCV".
#' @param Ker Kernel type.
#' @param ... Additional arguments passed to optimizer.
#'
#' @return A list with optimal bandwidth and CV/GCV score.
#'
#' @export
#' @examples
#' tt <- seq(0, 1, length.out = 50)
#' y <- sin(2 * pi * tt) + rnorm(50, sd = 0.1)
#' fd <- fdata(matrix(y, nrow = 1), argvals = tt)
#' result <- optim.np(fd, S.NW)
optim.np <- function(fdataobj, S.type, h.range = NULL, criterion = "GCV",
                     Ker = "norm", ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  tt <- fdataobj$argvals
  y <- as.vector(fdataobj$data[1, ])  # Use first curve for optimization

  # Default bandwidth range
  if (is.null(h.range)) {
    h_default <- h.default(tt)
    h.range <- c(h_default / 5, h_default * 5)
  }

  # Select criterion function
  crit_fun <- if (criterion == "CV") CV.S else GCV.S

  # Optimize
  result <- optimize(function(h) {
    crit_fun(S.type, tt, h, y, Ker = Ker)
  }, interval = h.range)

  list(
    h.opt = result$minimum,
    criterion = criterion,
    value = result$objective
  )
}
