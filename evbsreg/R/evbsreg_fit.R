#' Fit the Extreme-Value Birnbaum-Saunders Regression Model
#'
#' Fits the log-Extreme-Value Birnbaum-Saunders (log-EVBS) regression model
#' by joint maximum likelihood estimation. All parameters
#' \eqn{(\beta^\top, \alpha, \gamma)^\top} are estimated simultaneously
#' using the BFGS algorithm with an analytic score function. Standard
#' errors are computed from the analytic observed Fisher information
#' matrix, which is numerically stable even for ill-conditioned design
#' matrices.
#'
#' @param x A numeric design matrix of dimension \eqn{n \times p}. It
#'   \strong{must} include an intercept column (a column of ones). Each row
#'   corresponds to one observation and each column to one covariate.
#' @param t A numeric vector of length \eqn{n} containing the
#'   \strong{strictly positive} responses (e.g. wind gust speeds). Internally
#'   the model is fit to \code{log(t)}.
#'
#' @details
#' The EVBS regression model links the location parameter of the log-EVBS
#' distribution to a linear predictor \eqn{\mu_i = x_i^\top \beta}. The
#' shape parameter \eqn{\alpha > 0} controls dispersion and the tail-shape
#' parameter \eqn{\gamma} governs the Generalized Extreme Value tail
#' behaviour (Frechet for \eqn{\gamma>0}, Gumbel for \eqn{\gamma=0},
#' Weibull for \eqn{\gamma<0}).
#'
#' Initial values are obtained from \code{\link[stats]{lm.fit}} applied to
#' the log response, together with the moment-based starting point
#' \eqn{\alpha_0 = \sqrt{(4/n)\sum \sinh^2((y_i - x_i^\top \beta_0)/2)}}
#' and \eqn{\gamma_0 = 0.01}. Optimization uses \code{\link[stats]{optim}}
#' with \code{method = "BFGS"}, the analytic score, and tolerance
#' \code{reltol = 1e-12}.
#'
#' The observed Fisher information is assembled analytically (see the
#' package vignette and the paper's appendix) and inverted via a Cholesky
#' factorization, falling back to \code{\link{solve}} if the matrix is not
#' positive definite.
#'
#' @return A list of class \code{"evbsreg"} with components:
#' \describe{
#'   \item{\code{coeff}}{Numeric vector of length \eqn{p+2}: the full
#'     parameter vector \eqn{(\beta_0,\ldots,\beta_{p-1}, \alpha, \gamma)}.}
#'   \item{\code{betahat}}{Numeric vector of length \eqn{p}: the regression
#'     coefficients.}
#'   \item{\code{alphahat}}{Scalar: the estimated shape parameter
#'     \eqn{\hat\alpha}.}
#'   \item{\code{gamahat}}{Scalar: the estimated tail-shape parameter
#'     \eqn{\hat\gamma}.}
#'   \item{\code{stderrors}}{Numeric vector of length \eqn{p}: standard
#'     errors of the regression coefficients.}
#'   \item{\code{stderroralpha}}{Scalar: standard error of \eqn{\hat\alpha}.}
#'   \item{\code{stderrorgama}}{Scalar: standard error of \eqn{\hat\gamma}.}
#'   \item{\code{zstats}}{Numeric vector: Wald z-statistics for the
#'     regression coefficients.}
#'   \item{\code{pvalues}}{Numeric vector: two-sided p-values for the
#'     regression coefficients.}
#'   \item{\code{muhat}}{Numeric vector of length \eqn{n}: fitted linear
#'     predictor on the log scale.}
#'   \item{\code{xi1}, \code{xi2}}{Numeric vectors of length \eqn{n}: helper
#'     quantities \eqn{\xi_{i1}, \xi_{i2}} evaluated at the MLE.}
#'   \item{\code{observmatrix}}{Numeric matrix of dimension
#'     \eqn{(p+2)\times(p+2)}: the analytic Hessian
#'     \eqn{\ddot\ell(\hat\theta)} of the log-likelihood (negative definite
#'     at the maximum).}
#'   \item{\code{hessian}}{Numeric matrix: identical to
#'     \code{observmatrix}; provided under the conventional name. The
#'     observed Fisher information is \code{-hessian}.}
#'   \item{\code{inv}}{Numeric matrix of dimension \eqn{(p+2)\times(p+2)}:
#'     the inverse of the observed Fisher information \code{-hessian}, i.e.
#'     the asymptotic variance-covariance matrix of \eqn{\hat\theta}.}
#'   \item{\code{B}}{Numeric matrix of dimension \eqn{n\times n}: the
#'     influence matrix \eqn{B = \Delta^\top (-\ddot\ell)^{-1} \Delta} for
#'     the case-weight perturbation scheme, consumed by
#'     \code{\link{cnc_diagnostics}}.}
#'   \item{\code{nobs}}{Integer: the number of observations \eqn{n}.}
#'   \item{\code{npar}}{Integer: the number of parameters \eqn{p+2}.}
#' }
#'
#' The returned object has class \code{"evbsreg"} and has a
#' \code{\link{print.evbsreg}} method that displays a coefficient table.
#'
#' @references
#' Ospina, R., Lima, J. I. C., Barros, M., and Macedo, A. M. S. (2026).
#' Local influence diagnostics for the extreme-value Birnbaum-Saunders
#' regression model: methodology, validation, and application to anomalous
#' wind gusts. \emph{Submitted}.
#'
#' Leiva, V., Ferreira, M., Gomes, M. I., and Lillo, C. (2016). Extreme
#' value Birnbaum-Saunders regression models applied to environmental data.
#' \emph{Stochastic Environmental Research and Risk Assessment}, 30,
#' 1045--1058.
#'
#' Cook, R. D. (1986). Assessment of local influence.
#' \emph{Journal of the Royal Statistical Society, Series B}, 48, 133--169.
#'
#' @seealso \code{\link{cnc_diagnostics}} for influence diagnostics,
#'   \code{\link{rqrandomized}} for residuals, \code{\link{itajai}} for the
#'   example dataset.
#'
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure)
#' fit <- evbsreg.fit(X, itajai$wind)
#'
#' ## Parameter estimates and standard errors
#' round(fit$coeff, 4)
#' round(c(fit$stderrors, fit$stderroralpha, fit$stderrorgama), 4)
#'
#' @importFrom stats lm.fit optim pnorm
#' @export
evbsreg.fit <- function(x, t) {

  x <- as.matrix(x)
  y <- as.matrix(log(t))
  n <- length(y)

  # ---- Initial values (same as original code) ----
  ajuste <- lm.fit(x, y)
  beta   <- c(ajuste$coef)
  k      <- length(beta)
  alpha  <- sqrt((4 / n) * sum((sinh((y - x %*% beta) / 2))^2))
  gama   <- 0.01
  chute  <- c(beta, alpha, gama)

  # ---- Log-likelihood (gamma != 0 case) ----
  # Returns a large finite penalty (rather than NaN) when the optimizer
  # probes parameter values that fall outside the model support, i.e.
  # where 1 + gamma * xi2 <= 0 or alpha <= 0. This keeps BFGS on track
  # and silences spurious NaN warnings from log() of negative arguments.
  loglik <- function(z) {
    z1 <- z[1:k]; z2 <- z[k + 1]; z3 <- z[k + 2]
    if (z2 <= 0) return(-1e10)
    mu  <- x %*% z1
    xi1 <- (2 / z2) * cosh((y - mu) / 2)
    xi2 <- (2 / z2) * sinh((y - mu) / 2)
    arg <- 1 + z3 * xi2
    if (any(arg <= 0) || any(xi1 <= 0)) return(-1e10)
    val <- sum(-log(2) + log(xi1) +
                 (-1 - (1/z3)) * (log(arg)) -
                 arg^(-1/z3))
    if (!is.finite(val)) return(-1e10)
    val
  }

  # ---- Analytic score function ----
  escore <- function(z) {
    z1 <- z[1:k]; z2 <- z[k + 1]; z3 <- z[k + 2]
    mu  <- x %*% z1
    xi1 <- (2 / z2) * cosh((y - mu) / 2)
    xi2 <- (2 / z2) * sinh((y - mu) / 2)
    arg <- 1 + z3 * xi2
    # Outside the support the gradient is undefined; return zeros so BFGS
    # relies on the penalized objective to step back into the feasible region.
    if (z2 <= 0 || any(arg <= 0) || any(xi1 <= 0)) {
      return(rep(0, k + 2))
    }
    Ubeta  <- (1/2) * ((xi1 / arg) *
                         (1 + z3 - arg^(-1/z3)) -
                       (xi2 / xi1))
    Ualpha <- sum((1/z2) * (xi2 / arg) *
                    (1 + z3 - arg^(-1/z3)) - (1/z2))
    Ugama  <- sum(((-1/z3) * (xi2 / arg) *
                     (1 + z3 - arg^(-1/z3))) +
                    (1/(z3^2)) * (log(arg)) *
                    (1 - arg^(-1/z3)))
    g <- c(t(x) %*% Ubeta, Ualpha, Ugama)
    g[!is.finite(g)] <- 0
    g
  }

  # ---- BFGS optimization ----
  # The analytic observed information matrix is assembled below, so the
  # numerical Hessian from optim() is not requested (hessian = FALSE).
  # Boundary evaluations during the line search are handled by the
  # penalized objective; we additionally wrap the call in a local handler
  # so that no spurious NaN warning can reach the user. This does not
  # affect the optimization, which never accepts an out-of-support point.
  est <- withCallingHandlers(
    optim(chute, loglik, escore,
          method  = "BFGS",
          hessian = FALSE,
          control = list(fnscale = -1, maxit = 2000, reltol = 1e-12)),
    warning = function(w) {
      if (grepl("NaN|production|produced", conditionMessage(w)))
        invokeRestart("muffleWarning")
    }
  )

  if (est$convergence != 0)
    warning("evbsreg.fit: optimization did not converge")

  z <- list()
  coef     <- est$par[1:k]
  alphaest <- est$par[k + 1]
  gamaest  <- est$par[k + 2]

  z$coeff    <- c(coef, alphaest, gamaest)
  z$betahat  <- coef
  z$alphahat <- alphaest
  z$gamahat  <- gamaest

  muhat <- as.vector(x %*% coef)
  z$muhat <- muhat

  xi1hat <- (2 / alphaest) * cosh((y - muhat) / 2)
  xi2hat <- (2 / alphaest) * sinh((y - muhat) / 2)
  z$xi1  <- xi1hat
  z$xi2  <- xi2hat

  # ---- Helper quantities at the MLE ----
  q22hat <- xi2hat / (1 + gamaest * xi2hat)
  q12hat <- xi1hat / (1 + gamaest * xi2hat)
  pi2hat <- (1 + gamaest * xi2hat)^(-1/gamaest)
  qhat   <- xi2hat / xi1hat

  # ============================================================
  # Analytic observed Fisher information matrix (-L'')
  # Block expressions (Lima, Ospina, Barros, Macedo)
  # ============================================================

  # Lalphaalpha
  Lalpha <- sum((1/alphaest^2)
                - (2/(alphaest^2)) * q22hat * (1 + gamaest - pi2hat)
                + ((gamaest + 1) * (1/alphaest^2)) * (q22hat^2) *
                  (gamaest - pi2hat))

  # Lalphabeta (column vector by x rows -> Lalphabeta = t(x) %*% di_ab)
  di_ab <- ((-1/(2*alphaest)) * q12hat * (gamaest + 1 - pi2hat)
            + (1/(2*alphaest)) * (gamaest + 1) * q12hat * q22hat *
              (gamaest - pi2hat))
  Lalphabeta <- t(x) %*% di_ab
  Lbetaalpha <- t(di_ab) %*% x

  # Lbetabeta
  vi <- (1/4) * (1 - qhat^2) -
        (1/4) * q22hat * (1 + gamaest - pi2hat) +
        (1/4) * (q12hat^2) * (1 + gamaest) * (gamaest - pi2hat)
  V    <- diag(as.vector(vi))
  Lbeta <- t(x) %*% V %*% x

  # Lgamagama
  Lgama <- sum(((-2/gamaest^3) * log(1 + gamaest * xi2hat) +
                  (1/gamaest^2) * q22hat) * (1 - pi2hat)) +
           sum((1/gamaest^2) * log(1 + gamaest * xi2hat) *
                 ((1/gamaest) * q22hat * pi2hat -
                  (1/gamaest^2) * pi2hat * log(1 + gamaest * xi2hat))) +
           sum(((1/gamaest^2) * q22hat + (1/gamaest) * q22hat^2) *
                 (1 + gamaest - pi2hat)) +
           sum(((-1/gamaest) * q22hat) *
                 (1 + (1/gamaest) * q22hat * pi2hat -
                  (1/gamaest^2) * pi2hat * log(1 + gamaest * xi2hat)))

  # Lgamaalpha (= Lalphagama)
  Lgamaalpha <- sum((1/alphaest) * q22hat *
                      (1 - (1/gamaest^2) * pi2hat *
                         log(1 + gamaest * xi2hat))) +
                sum((-1/alphaest) * (1 + gamaest) * (q22hat^2) *
                      (1 - (1/gamaest) * pi2hat))

  # Lgamabeta
  si <- (1/2) * q12hat *
          (1 - (1/gamaest^2) * pi2hat * log(1 + gamaest * xi2hat)) -
        (1/2) * q12hat * q22hat * (1 + gamaest) *
          (1 - (1/gamaest) * pi2hat)
  Lgamabeta <- t(x) %*% si
  Lbetagama <- t(si) %*% x

  # Assemble observed information matrix.
  # Notation in the helper terms above:
  #   Lbeta       : k x k
  #   Lbetaalpha  : 1 x k  (= t(di_ab) %*% x)
  #   Lbetagama   : 1 x k  (= t(si) %*% x)
  #   Lalphabeta  : k x 1  (= t(x) %*% di_ab)
  #   Lalpha      : scalar
  #   Lgamaalpha  : scalar
  #   Lgamabeta   : k x 1  (= t(x) %*% si)
  #   Lgama       : scalar
  L1 <- rbind(Lbeta,      Lbetaalpha, Lbetagama)
  L2 <- rbind(Lalphabeta, Lalpha,     Lgamaalpha)
  L3 <- rbind(Lgamabeta,  Lgamaalpha, Lgama)
  observmatrix <- cbind(L1, L2, L3)

  z$observmatrix <- observmatrix
  # observmatrix is the Hessian of the log-likelihood (negative definite
  # at the MLE); the observed Fisher information is -observmatrix.
  z$hessian <- observmatrix

  # ---- Inverse of observed Fisher information ----
  # Invert the observed information (-Hessian) via Cholesky; fall back to a
  # general solve() if the matrix is not numerically positive definite.
  invobs <- tryCatch(
    chol2inv(chol(-observmatrix)),
    error = function(e) solve(-observmatrix)
  )
  z$inv <- invobs

  z$stderrors     <- sqrt(diag(invobs))[1:k]
  z$stderroralpha <- sqrt(diag(invobs))[k + 1]
  z$stderrorgama  <- sqrt(diag(invobs))[k + 2]

  z$zstats  <- coef / z$stderrors
  z$pvalues <- 2 * (1 - pnorm(abs(coef / z$stderrors)))

  # ---- Case-weight perturbation matrix Delta ----
  bi <- (1/2) * (q12hat * (gamaest + 1 - pi2hat) - (xi2hat / xi1hat))
  ci <- (-1/alphaest) + (1/alphaest) * q22hat * (gamaest + 1 - pi2hat)
  di <- (1/gamaest^2) * (1 - pi2hat) * (log(1 + gamaest * xi2hat)) -
        (1/gamaest)   * q22hat * (gamaest + 1 - pi2hat)

  Deltabetapc  <- t(x) %*% diag(as.vector(bi))
  Deltaalphapc <- t(ci)
  Deltagamapc  <- t(di)
  Deltapc      <- rbind(Deltabetapc, Deltaalphapc, Deltagamapc)

  # ---- Influence matrix B = Delta' inv(-Hessian) Delta ----
  z$B <- t(Deltapc) %*% invobs %*% Deltapc

  z$nobs <- n
  z$npar <- k + 2
  class(z) <- "evbsreg"
  z
}


#' Print Method for EVBS Regression Fits
#'
#' Compactly prints the parameter estimates, standard errors, and Wald
#' tests of an object returned by \code{\link{evbsreg.fit}}.
#'
#' @param x An object of class \code{"evbsreg"}.
#' @param digits Number of significant digits (default 4).
#' @param ... Further arguments passed to \code{\link{print.default}}.
#'
#' @return The object \code{x}, invisibly. Called for the side effect of
#'   printing a coefficient table to the console.
#'
#' @seealso \code{\link{evbsreg.fit}}.
#'
#' @examples
#' data(itajai)
#' fit <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
#' print(fit)
#'
#' @export
print.evbsreg <- function(x, digits = 4, ...) {
  k <- length(x$betahat)
  est <- c(x$betahat, x$alphahat, x$gamahat)
  se  <- c(x$stderrors, x$stderroralpha, x$stderrorgama)
  zv  <- c(x$zstats, NA, NA)
  pv  <- c(x$pvalues, NA, NA)
  nm  <- c(paste0("beta", 0:(k - 1)), "alpha", "gamma")
  tab <- data.frame(Estimate = round(est, digits),
                    Std.Error = round(se, digits),
                    z.value = round(zv, digits),
                    p.value = round(pv, digits),
                    row.names = nm)
  cat("Extreme-Value Birnbaum-Saunders regression\n")
  cat(sprintf("n = %d observations, %d parameters\n\n", x$nobs, x$npar))
  print(tab, ...)
  invisible(x)
}
