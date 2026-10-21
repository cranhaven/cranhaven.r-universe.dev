#' Density of the Extreme-Value Birnbaum-Saunders distribution
#'
#' Probability density function of the EVBS distribution on the response
#' (positive) scale. The EVBS variate \eqn{T} satisfies
#' \eqn{Z = (2/\alpha)\,\sinh\{(\log T - \eta)/2\} \sim \mathrm{GEV}(0,1,\gamma)}.
#'
#' @param t Vector of positive quantiles.
#' @param eta Location on the log scale, typically \eqn{x^\top \beta}.
#' @param alpha Positive shape (scale) parameter.
#' @param gama Extreme-value shape parameter.
#' @param log Logical; if \code{TRUE}, the log-density is returned.
#'
#' @details
#' The extreme-value index of the \emph{response} \eqn{T} is
#' \eqn{2\gamma}, not \eqn{\gamma}. The transformation preserves the
#' max-domain of attraction but doubles the tail index, because
#' \eqn{T \approx \beta\alpha^2 Z^2} in the upper tail.
#'
#' @return A numeric vector of (log-)density values.
#' @examples
#' devbs(c(10, 20, 30), eta = 2.58, alpha = 0.19, gama = -0.16)
#' @export
devbs <- function(t, eta, alpha, gama, log = FALSE) {
  stopifnot(alpha > 0)
  t <- as.numeric(t)
  out <- rep(-Inf, length(t))
  ok <- is.finite(t) & t > 0
  if (!any(ok)) return(if (log) out else exp(out))
  z <- (2 / alpha) * sinh((base::log(t[ok]) - eta) / 2)
  if (abs(gama) < 1e-8) {
    lg <- -z - exp(-z)
  } else {
    u <- 1 + gama * z
    lg <- rep(-Inf, length(z))
    pos <- u > 0
    lg[pos] <- -(1 / gama + 1) * base::log(u[pos]) - u[pos]^(-1 / gama)
  }
  ljac <- base::log(cosh((base::log(t[ok]) - eta) / 2)) -
    base::log(alpha * t[ok])
  out[ok] <- lg + ljac
  if (log) out else exp(out)
}

#' Distribution function of the Extreme-Value Birnbaum-Saunders distribution
#'
#' @inheritParams devbs
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are
#'   \eqn{P(T \le t)}, otherwise \eqn{P(T > t)}.
#'
#' @return A numeric vector of probabilities.
#' @examples
#' pevbs(30, eta = 2.58, alpha = 0.19, gama = -0.16, lower.tail = FALSE)
#' @export
pevbs <- function(t, eta, alpha, gama, lower.tail = TRUE) {
  stopifnot(alpha > 0)
  t <- as.numeric(t)
  p <- rep(NA_real_, length(t))
  ok <- is.finite(t) & t > 0
  z <- (2 / alpha) * sinh((log(t[ok]) - eta) / 2)
  if (abs(gama) < 1e-8) {
    p[ok] <- exp(-exp(-z))
  } else {
    u <- 1 + gama * z
    pk <- numeric(length(z))
    pk[u <= 0] <- if (gama > 0) 0 else 1
    pos <- u > 0
    pk[pos] <- exp(-u[pos]^(-1 / gama))
    p[ok] <- pk
  }
  p[is.finite(t) & t <= 0] <- 0
  if (lower.tail) p else 1 - p
}

#' Quantile function of the Extreme-Value Birnbaum-Saunders distribution
#'
#' Exact quantile function on the response scale. This is the function needed
#' to compute return levels; the generalized extreme-value return-level formula
#' does \strong{not} apply to the EVBS response, since the response is a
#' monotone transformation of a GEV variate rather than a GEV variate itself.
#'
#' @inheritParams devbs
#' @param p Vector of probabilities in \eqn{(0,1)}.
#'
#' @return A numeric vector of quantiles.
#' @examples
#' # 50-year return level from monthly maxima: p = 1 - 1/(12*50)
#' qevbs(1 - 1 / 600, eta = 2.58, alpha = 0.19, gama = -0.16)
#' @export
qevbs <- function(p, eta, alpha, gama) {
  stopifnot(alpha > 0, all(p > 0 & p < 1))
  zp <- if (abs(gama) < 1e-8) {
    -log(-log(p))
  } else {
    ((-log(p))^(-gama) - 1) / gama
  }
  exp(eta + 2 * asinh(alpha * zp / 2))
}

#' Finite upper endpoint of the EVBS distribution
#'
#' When \eqn{\gamma < 0} the EVBS lies in the Weibull max-domain of attraction
#' and its support is bounded above. The endpoint is the largest response the
#' fitted model regards as possible, and is therefore the quantity of primary
#' interest in design applications. It is also highly sensitive to individual
#' observations, which is what the local influence diagnostics in this package
#' are designed to detect.
#'
#' @inheritParams devbs
#'
#' @return The finite upper endpoint, or \code{Inf} when \eqn{\gamma \ge 0}.
#' @examples
#' evbs_endpoint(eta = 2.58, alpha = 0.19, gama = -0.16)
#' evbs_endpoint(eta = 2.58, alpha = 0.19, gama = 0.10) # Inf
#' @export
evbs_endpoint <- function(eta, alpha, gama) {
  stopifnot(alpha > 0)
  if (gama >= 0) return(Inf)
  exp(eta + 2 * asinh(alpha * (-1 / gama) / 2))
}

#' Return level and expected shortfall for the EVBS regression model
#'
#' Computes the \code{period}-observation return level and, optionally, the
#' conditional tail expectation (expected shortfall) at a given linear
#' predictor value.
#'
#' @param object An object of class \code{evbsreg} from \code{\link{evbsreg.fit}}.
#' @param x Covariate vector (including the intercept) at which to evaluate.
#' @param period Return period, expressed in the units of one observation. For
#'   monthly maxima and a \eqn{T}-year return level, use \code{period = 12 * T}.
#' @param es Logical; if \code{TRUE}, also return the expected shortfall.
#'
#' @return A named numeric vector with the return level and, if requested, the
#'   expected shortfall.
#' @examples
#' data(itajai)
#' fit <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
#' # 50-year return level at mean pressure, from monthly maxima
#' evbs_return_level(fit, x = c(1, mean(itajai$pressure)),
#'                   period = 12 * 50, es = TRUE)
#' @export
evbs_return_level <- function(object, x, period, es = FALSE) {
  stopifnot(inherits(object, "evbsreg") || is.list(object))
  beta <- object$betahat
  alpha <- object$alphahat
  gama <- object$gamahat
  eta <- sum(as.numeric(x) * beta)
  p <- 1 - 1 / period
  rl <- qevbs(p, eta, alpha, gama)
  out <- c(return_level = rl)
  if (es) {
    val <- stats::integrate(
      function(u) qevbs(u, eta, alpha, gama),
      lower = p, upper = 1 - 1e-10, subdivisions = 2000L
    )$value / (1 - p)
    out <- c(out, expected_shortfall = val)
  }
  out
}
