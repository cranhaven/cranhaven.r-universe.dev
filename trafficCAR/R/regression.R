#' Gibbs update for beta in y = X beta + x + eps
#'
#' Model:
#'   y | beta, x, sigma2 ~ N(X beta + x, sigma2 I)
#'   beta ~ N(b0, B0)
#'
#' @param y numeric length n
#' @param X numeric n x p
#' @param x numeric length n (latent effect)
#' @param sigma2 positive scalar
#' @param b0 numeric length p prior mean
#' @param B0 positive definite p x p prior covariance
#'
#' @importFrom stats rnorm
#' @return numeric length p beta draw
#' @keywords internal
update_beta_gaussian <- function(y, X, x, sigma2, b0, B0) {
  ## basic type checks
  if (!is.numeric(y)) stop("`y` must be numeric.")
  if (typeof(y) != "double") stop("`y` must be a double vector.")
  if (!is.matrix(X) || !is.numeric(X))
    stop("`X` must be a numeric matrix.")

  n <- length(y)
  if (nrow(X) != n)
    stop("`X` must have nrow(X) == length(y).")
  p <- ncol(X)

  if (!is.numeric(x) || length(x) != n)
    stop("`x` must be numeric length length(y).")

  ## finiteness checks
  if (any(!is.finite(y))) stop("`y` must be finite.")
  if (any(!is.finite(x))) stop("`x` must be finite.")
  if (any(!is.finite(X))) stop("`X` must be finite.")

  if (!is.numeric(sigma2) || length(sigma2) != 1 ||
      !is.finite(sigma2) || sigma2 <= 0)
    stop("`sigma2` must be a positive scalar.")

  if (!is.numeric(b0) || length(b0) != p)
    stop("`b0` must be numeric length ncol(X).")
  if (!is.matrix(B0) || !is.numeric(B0) || any(dim(B0) != c(p, p)))
    stop("`B0` must be a numeric p x p matrix.")
  if (!isTRUE(all.equal(B0, t(B0))))
    stop("`B0` must be symmetric.")

  ## PD check only (no unused assignment)
  tryCatch(
    chol(B0),
    error = function(e) stop("`B0` must be positive definite.")
  )

  ## numeric stabilization: scale columns of X
  scale <- apply(abs(X), 2, max)
  scale[!is.finite(scale) | scale == 0] <- 1
  Xs <- sweep(X, 2, scale, "/")

  ## transform prior to scaled space
  b0s <- as.numeric(scale * b0)
  Bs0 <- (scale * B0) * rep(scale, each = p)  # D %*% B0 %*% D

  if (!isTRUE(all.equal(Bs0, t(Bs0))))
    Bs0 <- 0.5 * (Bs0 + t(Bs0))

  Bs0_ch <- tryCatch(
    chol(Bs0),
    error = function(e)
      stop("Scaled `B0` not positive definite (numerical issue).")
  )
  Bs0_inv <- chol2inv(Bs0_ch)

  ## posterior precision
  XtX <- crossprod(Xs)
  Vinv <- XtX / sigma2 + Bs0_inv
  if (!isTRUE(all.equal(Vinv, t(Vinv))))
    Vinv <- 0.5 * (Vinv + t(Vinv))

  R <- tryCatch(
    chol(Vinv),
    error = function(e) stop("Posterior covariance not invertible.")
  )

  ## posterior mean via Cholesky solves
  rhs <- crossprod(Xs, (y - x)) / sigma2 + Bs0_inv %*% b0s
  m <- backsolve(R, forwardsolve(t(R), rhs))

  ## sample in scaled space
  z <- rnorm(p)
  delta <- backsolve(R, z)
  beta_s <- as.numeric(m + delta)

  ## transform back
  as.numeric(beta_s / scale)
}



#' Optional Gibbs update for sigma2 with Inv-Gamma prior
#'
#' Prior: sigma2 ~ Inv-Gamma(a0, b0)  (shape a0, scale b0)
#' Conditional: sigma2 | rest ~ Inv-Gamma(a0 + n/2, b0 + 0.5 * RSS)
#' @importFrom stats rgamma
#' @keywords internal
update_sigma2_ig <- function(y, X, beta, x, a0, b0) {
  if (!is.numeric(y)) stop("`y` must be numeric.")
  if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix.")
  if (!is.numeric(beta)) stop("`beta` must be numeric.")
  if (!is.numeric(x)) stop("`x` must be numeric.")

  n <- length(y)
  if (nrow(X) != n) stop("`X` must have nrow(X) == length(y).")
  if (length(x) != n) stop("`x` must be length length(y).")
  if (ncol(X) != length(beta)) stop("`beta` length must match ncol(X).")

  if (any(!is.finite(y))) stop("`y` must be finite.")
  if (any(!is.finite(X))) stop("`X` must be finite.")
  if (any(!is.finite(beta))) stop("`beta` must be finite.")
  if (any(!is.finite(x))) stop("`x` must be finite.")

  if (!is.numeric(a0) || length(a0) != 1 || !is.finite(a0) || a0 <= 0) stop("`a0` must be > 0.")
  if (!is.numeric(b0) || length(b0) != 1 || !is.finite(b0) || b0 <= 0) stop("`b0` must be > 0.")

  mu <- as.numeric(X %*% beta + x)
  rss <- sum((y - mu)^2)

  shape <- a0 + n / 2
  rate  <- b0 + 0.5 * rss

  1 / rgamma(1, shape = shape, rate = rate)
}
