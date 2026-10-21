#' Individual score contributions for the GEV regression model
#'
#' Returns the matrix whose \eqn{i}th row is the score vector of the \eqn{i}th
#' observation, \eqn{s_i(\theta) = \partial \ell_i / \partial \theta}, for the
#' generalized extreme-value regression model with linear location,
#' \eqn{Y_i \sim \mathrm{GEV}(\mu_i, \sigma, \xi)} and
#' \eqn{\mu_i = x_i^\top \beta}.
#'
#' @details
#' This matrix is the key to generalizing local influence diagnostics beyond a
#' single model class. Under the case-weight perturbation scheme the perturbed
#' log-likelihood is \eqn{\ell(\theta \mid \omega) = \sum_i \omega_i
#' \ell_i(\theta)}, so the perturbation matrix is
#' \deqn{\Delta = \frac{\partial^2 \ell(\theta \mid \omega)}{\partial \theta \,
#' \partial \omega^\top} = [\, s_1(\theta) \; \cdots \; s_n(\theta) \,],}
#' evaluated at the maximum likelihood estimate. That is, \eqn{\Delta} is simply
#' the matrix of individual score contributions. This holds for \emph{any}
#' likelihood model, which is why the conformal normal curvature framework
#' requires nothing beyond the score and the observed information.
#'
#' With \eqn{z_i = (y_i - \mu_i)/\sigma}, \eqn{u_i = 1 + \xi z_i > 0} and
#' \eqn{w_i = u_i^{-1/\xi}}, the components are
#' \deqn{\partial \ell_i / \partial \mu_i = (\xi + 1 - w_i)/(\sigma u_i),}
#' \deqn{\partial \ell_i / \partial \sigma = -1/\sigma + z_i(\xi + 1 - w_i)/(\sigma u_i),}
#' \deqn{\partial \ell_i / \partial \xi = \xi^{-2}(1 - w_i)\log u_i -
#' (z_i/u_i)\{1 + (1 - w_i)/\xi\}.}
#'
#' @param y Response vector.
#' @param X Design matrix (including the intercept column).
#' @param beta Regression coefficients for the location parameter.
#' @param sigma Positive scale parameter.
#' @param xi Extreme-value shape parameter.
#'
#' @return An \eqn{n \times (p+2)} matrix of individual score contributions.
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure - mean(itajai$pressure))
#' fit <- gevreg.fit(X, itajai$wind)
#' S <- gev_scores(itajai$wind, X, fit$beta, fit$sigma, fit$xi)
#' colSums(S) # approximately zero at the MLE
#' @export
gev_scores <- function(y, X, beta, sigma, xi) {
  mu <- as.vector(X %*% beta)
  z <- (y - mu) / sigma
  u <- 1 + xi * z
  u[u <= 0] <- NA_real_
  w <- u^(-1 / xi)
  dmu <- (xi + 1 - w) / (sigma * u)
  dsg <- -1 / sigma + z * (xi + 1 - w) / (sigma * u)
  dxi <- (1 / xi^2) * (1 - w) * log(u) - (z / u) * (1 + (1 - w) / xi)
  S <- cbind(X * dmu, dsg, dxi)
  colnames(S) <- c(paste0("beta", seq_len(ncol(X)) - 1L), "sigma", "xi")
  S
}

#' Maximum likelihood fit of the GEV regression model
#'
#' Fits \eqn{Y_i \sim \mathrm{GEV}(\mu_i, \sigma, \xi)} with
#' \eqn{\mu_i = x_i^\top \beta} by maximum likelihood, using the analytic
#' gradient and several starting values for the shape parameter.
#'
#' Centring the covariates is strongly recommended: the GEV likelihood is
#' poorly conditioned when covariates are far from the origin, and an
#' uncentred fit may fail to converge without any warning.
#'
#' @param X Design matrix (including the intercept column).
#' @param y Response vector.
#' @param xi_start Vector of starting values for the shape parameter.
#'
#' @return A list with the estimates, the log-likelihood, the observed
#'   information (Hessian of the negative log-likelihood) and the convergence
#'   code.
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure - mean(itajai$pressure))
#' gevreg.fit(X, itajai$wind)
#' @export
gevreg.fit <- function(X, y, xi_start = c(-0.2, -0.05, 0.05, 0.2)) {
  X <- as.matrix(X)
  p <- ncol(X)
  negll <- function(par) {
    b <- par[1:p]; s <- exp(par[p + 1]); x <- par[p + 2]
    z <- (y - as.vector(X %*% b)) / s
    u <- 1 + x * z
    if (any(u <= 0)) return(1e10)
    -sum(-log(s) - (1 + 1 / x) * log(u) - u^(-1 / x))
  }
  grad <- function(par) {
    b <- par[1:p]; s <- exp(par[p + 1]); x <- par[p + 2]
    S <- gev_scores(y, X, b, s, x)
    g <- colSums(S, na.rm = TRUE)
    g[p + 1] <- g[p + 1] * s
    -g
  }
  best <- NULL
  for (x0 in xi_start) {
    st <- c(mean(y), rep(0, p - 1), log(stats::sd(y)), x0)
    o <- try(stats::optim(st, negll, grad, method = "BFGS",
                          control = list(maxit = 20000, reltol = 1e-13),
                          hessian = TRUE), silent = TRUE)
    if (!inherits(o, "try-error") && (is.null(best) || o$value < best$value)) {
      best <- o
    }
  }
  if (is.null(best)) stop("GEV fit failed to converge from all starting values.")
  structure(list(
    beta = best$par[1:p], sigma = exp(best$par[p + 1]), xi = best$par[p + 2],
    loglik = -best$value, hessian = best$hessian, par = best$par,
    convergence = best$convergence, X = X, y = y
  ), class = "gevreg")
}

#' Conformal normal curvature diagnostics for the GEV regression model
#'
#' Local influence diagnostics under case-weight perturbation for the GEV
#' regression model, obtained from the individual score contributions (see
#' \code{\link{gev_scores}}) and the observed information.
#'
#' @param object An object of class \code{gevreg} from \code{\link{gevreg.fit}}.
#'
#' @return A list with the normalized aggregate contributions \code{Bj}, the
#'   threshold \code{2/n}, and the indices of the flagged observations.
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure - mean(itajai$pressure))
#' fit <- gevreg.fit(X, itajai$wind)
#' d <- cnc_diagnostics_gev(fit)
#' head(order(d$Bj, decreasing = TRUE))
#' @export
cnc_diagnostics_gev <- function(object) {
  stopifnot(inherits(object, "gevreg"))
  X <- object$X; y <- object$y
  p <- ncol(X); n <- length(y)
  S <- gev_scores(y, X, object$beta, object$sigma, object$xi)
  S[is.na(S)] <- 0
  S[, p + 1] <- S[, p + 1] * object$sigma  # chain rule for log(sigma)
  Delta <- t(S)                            # (p+2) x n : the perturbation matrix
  Hinv <- solve(object$hessian)
  Fmat <- t(Delta) %*% Hinv %*% Delta
  dF <- abs(diag(Fmat))
  Bj <- dF / sum(dF)
  thr <- 2 / n
  list(Bj = Bj, threshold = thr, flagged = which(Bj > thr), n = n)
}
