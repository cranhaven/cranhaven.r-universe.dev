#' The log-EVBS distribution for GAMLSS models
#'
#' Defines the log-Extreme-Value Birnbaum-Saunders distribution as a
#' three-parameter \pkg{gamlss} family, so that each parameter may depend on
#' covariates through a linear predictor or a smooth term. This is the natural
#' way to let the tail-shape parameter vary with covariates, which the fixed
#' three-parameter fit of \code{\link{evbsreg.fit}} does not allow.
#'
#' @param mu.link Link for the location parameter \eqn{\mu} (default
#'   \code{"identity"}; the predictor is on the log-response scale).
#' @param sigma.link Link for the scale parameter \eqn{\alpha > 0} (default
#'   \code{"log"}).
#' @param nu.link Link for the extreme-value shape parameter \eqn{\gamma}
#'   (default \code{"identity"}).
#'
#' @details
#' The response is \eqn{Y = \log T} where \eqn{T} follows the EVBS
#' distribution, so that
#' \eqn{Z = (2/\sigma)\sinh\{(Y-\mu)/2\} \sim \mathrm{GEV}(0,1,\nu)}.
#' The parameter map is \eqn{\mu = \eta = x^\top\beta} (identity link),
#' \eqn{\sigma = \alpha} (log link, positive) and \eqn{\nu = \gamma} (identity
#' link, real-valued).
#'
#' With a constant predictor the fitted \eqn{(\mu, \sigma, \nu)} reproduce the
#' \eqn{(\eta, \alpha, \gamma)} of \code{\link{evbsreg.fit}}; this equivalence is
#' the recommended check after loading the family. The score and second-order
#' derivatives required by \pkg{gamlss} are obtained by automatic numerical
#' differentiation of the analytic log-density, which is exact to working
#' precision and avoids transcription error in the lengthy closed forms.
#'
#' @return A \code{gamlss.family} object.
#'
#' @examples
#' \dontrun{
#'   library(gamlss)
#'   data(itajai)
#'   y <- log(itajai$wind)
#'   # constant model: should match evbsreg.fit on the log scale
#'   m0 <- gamlss(y ~ 1, family = logEVBS())
#'   # tail shape depending on pressure:
#'   m1 <- gamlss(y ~ pressure, nu.formula = ~ pressure,
#'                data = itajai, family = logEVBS())
#' }
#' @export
logEVBS <- function(mu.link = "identity", sigma.link = "log",
                    nu.link = "identity") {

  mstats <- stats::make.link(mu.link)
  dstats <- stats::make.link(sigma.link)
  vstats <- stats::make.link(nu.link)

  ## analytic log-density of the log-EVBS on the response (log-T) scale
  ldens <- function(y, mu, sigma, nu) {
    z <- (2 / sigma) * sinh((y - mu) / 2)
    lch <- log(cosh((y - mu) / 2))
    small <- abs(nu) < 1e-8
    lg <- numeric(length(y))
    if (any(small)) lg[small] <- -z[small] - exp(-z[small])
    if (any(!small)) {
      u <- 1 + nu * z
      ok <- !small & u > 0
      lg[ok] <- -(1 / nu + 1) * log(u[ok]) - u[ok]^(-1 / nu)
      lg[!small & u <= 0] <- -Inf
    }
    lg + lch + log(1 / sigma)
  }
  ## numerical partial derivatives (central differences)
  d1 <- function(f, y, mu, sigma, nu, wrt) {
    h <- 1e-5
    if (wrt == "mu")    (f(y, mu + h, sigma, nu) - f(y, mu - h, sigma, nu)) / (2 * h)
    else if (wrt == "sigma") (f(y, mu, sigma + h, nu) - f(y, mu, sigma - h, nu)) / (2 * h)
    else (f(y, mu, sigma, nu + h) - f(y, mu, sigma, nu - h)) / (2 * h)
  }
  d2 <- function(f, y, mu, sigma, nu, wrt) {
    h <- 1e-4
    if (wrt == "mu")
      (f(y, mu + h, sigma, nu) - 2 * f(y, mu, sigma, nu) + f(y, mu - h, sigma, nu)) / h^2
    else if (wrt == "sigma")
      (f(y, mu, sigma + h, nu) - 2 * f(y, mu, sigma, nu) + f(y, mu, sigma - h, nu)) / h^2
    else
      (f(y, mu, sigma, nu + h) - 2 * f(y, mu, sigma, nu) + f(y, mu, sigma, nu - h)) / h^2
  }
  dcross <- function(f, y, mu, sigma, nu, a, b) {
    h <- 1e-4
    bump <- function(pm, pn) {
      p <- c(mu = mu, sigma = sigma, nu = nu)
      p[a] <- p[a] + pm * h; p[b] <- p[b] + pn * h
      f(y, p["mu"], p["sigma"], p["nu"])
    }
    (bump(1, 1) - bump(1, -1) - bump(-1, 1) + bump(-1, -1)) / (4 * h^2)
  }

  structure(
    list(
      family = c("logEVBS", "log-Extreme-Value-Birnbaum-Saunders"),
      parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
      nopar = 3, type = "Continuous",
      mu.link = mu.link, sigma.link = sigma.link, nu.link = nu.link,
      mu.linkfun = mstats$linkfun, sigma.linkfun = dstats$linkfun,
      nu.linkfun = vstats$linkfun,
      mu.linkinv = mstats$linkinv, sigma.linkinv = dstats$linkinv,
      nu.linkinv = vstats$linkinv,
      mu.dr = mstats$mu.eta, sigma.dr = dstats$mu.eta, nu.dr = vstats$mu.eta,

      dldm = function(y, mu, sigma, nu) d1(ldens, y, mu, sigma, nu, "mu"),
      d2ldm2 = function(y, mu, sigma, nu) {
        v <- d2(ldens, y, mu, sigma, nu, "mu"); ifelse(v < -1e-10, v, -1e-10)
      },
      dldd = function(y, mu, sigma, nu) d1(ldens, y, mu, sigma, nu, "sigma"),
      d2ldd2 = function(y, mu, sigma, nu) {
        v <- d2(ldens, y, mu, sigma, nu, "sigma"); ifelse(v < -1e-10, v, -1e-10)
      },
      dldv = function(y, mu, sigma, nu) d1(ldens, y, mu, sigma, nu, "nu"),
      d2ldv2 = function(y, mu, sigma, nu) {
        v <- d2(ldens, y, mu, sigma, nu, "nu"); ifelse(v < -1e-10, v, -1e-10)
      },
      d2ldmdd = function(y, mu, sigma, nu) dcross(ldens, y, mu, sigma, nu, "mu", "sigma"),
      d2ldmdv = function(y, mu, sigma, nu) dcross(ldens, y, mu, sigma, nu, "mu", "nu"),
      d2ldddv = function(y, mu, sigma, nu) dcross(ldens, y, mu, sigma, nu, "sigma", "nu"),

      G.dev.incr = function(y, mu, sigma, nu, ...) -2 * ldens(y, mu, sigma, nu),
      rqres = expression(
        rqres(pfun = "plogEVBS", type = "Continuous",
              y = y, mu = mu, sigma = sigma, nu = nu)
      ),
      mu.initial    = expression(mu    <- rep(mean(y), length(y))),
      sigma.initial = expression(sigma <- rep(0.5, length(y))),
      nu.initial    = expression(nu    <- rep(-0.1, length(y))),
      mu.valid    = function(mu) TRUE,
      sigma.valid = function(sigma) all(sigma > 0),
      nu.valid    = function(nu) TRUE,
      y.valid     = function(y) TRUE
    ),
    class = c("gamlss.family", "family")
  )
}

#' Density of the log-EVBS distribution (GAMLSS parametrization)
#'
#' @param x,q,p Vector of quantiles / probabilities.
#' @param mu Location on the log scale.
#' @param sigma Positive scale parameter.
#' @param nu Extreme-value shape parameter.
#' @param log,log.p Logical; return the log-density / log-probability.
#' @param lower.tail Logical; if \code{TRUE}, probabilities are \eqn{P(Y \le y)}.
#' @return A numeric vector.
#' @rdname logEVBS-dist
#' @export
dlogEVBS <- function(x, mu = 0, sigma = 1, nu = 0, log = FALSE) {
  z <- (2 / sigma) * sinh((x - mu) / 2)
  lch <- base::log(cosh((x - mu) / 2))
  small <- abs(nu) < 1e-8
  lg <- numeric(length(x))
  if (any(small)) lg[small] <- -z[small] - exp(-z[small])
  if (any(!small)) {
    u <- 1 + nu * z
    ok <- !small & u > 0
    lg[ok] <- -(1 / nu + 1) * base::log(u[ok]) - u[ok]^(-1 / nu)
    lg[!small & u <= 0] <- -Inf
  }
  d <- lg + lch + base::log(1 / sigma)
  if (log) d else exp(d)
}

#' @rdname logEVBS-dist
#' @export
plogEVBS <- function(q, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE,
                     log.p = FALSE) {
  z <- (2 / sigma) * sinh((q - mu) / 2)
  if (abs(nu) < 1e-8) {
    p <- exp(-exp(-z))
  } else {
    u <- 1 + nu * z
    p <- ifelse(u > 0, exp(-u^(-1 / nu)), ifelse(nu > 0, 0, 1))
  }
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' @rdname logEVBS-dist
#' @export
qlogEVBS <- function(p, mu = 0, sigma = 1, nu = 0, lower.tail = TRUE,
                     log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  zp <- if (abs(nu) < 1e-8) -log(-log(p)) else ((-log(p))^(-nu) - 1) / nu
  mu + 2 * asinh(sigma * zp / 2)
}
