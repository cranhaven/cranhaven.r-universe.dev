#' Conditional posterior distribution of latent U
#'
#' This function simulates from the conditional posterior distribution of the
#' latent U.
#'
#' For internal use.
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(ut, n = 200, r = 20, alpha = 1, kappa = 1, gama = 1 / 2,
#'          delta = 2) {
#'   w <- ut
#'   ratio <- NaN
#'   while (is.nan(ratio)) {
#'     v <- ustar <- rgamma(1, shape = delta, rate = delta / ut)
#'     vw <- v / w
#'     vb <- v + kappa
#'     wb <- w + kappa
#'     A <- vw^(n - 2 * delta)
#'     B <- (vb / wb)^(r * gama - n)
#'     D <- vb^gama - wb^gama
#'     E <- 1 / vw - vw
#'     ratio <- A * B * exp(-alpha / gama * D - delta * E)
#'   }
#'   p <- min(1, ratio)
#'   u <- ifelse(runif(1) <= p, ustar, ut)
#'   return(u)
#' }
gs3 <-
  function(ut, n, r, alpha, kappa, gama, delta) {
    w <- ut
    ratio <- NaN
    while (is.nan(ratio)) {
      v <- ustar <- rgamma(1, shape = delta, rate = delta / ut)
      vw <- v / w
      vb <- v + kappa
      wb <- w + kappa
      A <- vw^(n - 2 * delta)
      B <- (vb / wb)^(r * gama - n)
      D <- vb^gama - wb^gama
      E <- 1 / vw - vw
      ratio <- A * B * exp(-alpha / gama * D - delta * E)
    }
    p <- min(1, ratio)
    u <- ifelse(runif(1) <= p, ustar, ut)
    return(u)
  }

#' Target logdensity of U given the data
#'
#' @keywords internal
#'
logf_u_cond_y <- function(u, n, r, gamma, kappa, a) {
  (n - 1) * log(u) + (r * gamma - n) * log(u + kappa) - a / gamma * (u + kappa)^gamma
}

#' Contribution of the target logdensity of logU to the Metropolis-Hastings ratio
#'
#' @keywords internal
#'
logf_logu_cond_y <- function(logu, n, r, gamma, kappa, a) {
  logu + logf_u_cond_y(u = exp(logu), n = n, r = r, gamma = gamma, kappa = kappa, a = a)
}

#' Contribution of the proposal kernel logdensity to the Metropolis-Hastings ratio
#'
#' @keywords internal
#'
logdprop_logu <- function(logu_prime, logu, delta) {
  dnorm(x = logu_prime, mean = logu, sd = delta, log = T)
}

#' Proposal distribution for logU
#'
#' This function makes a proposal for a new value of logU
#'
#' @inheritParams logacceptance_ratio_logu
#' @keywords internal
#'
rprop_logu <- function(logu, delta) {
  rnorm(n = 1, mean = logu, sd = delta)
}

#' Metropolis-Hastings ratio for the conditional of logU
#'
#' This function computes the Metropolis-Hastings ratio to decide whether to accept or reject a new value for logU.
#'
#' @param logu Real, log of the latent variable U at the current iteration.
#' @param logu_prime Real, log of the new proposed latent variable U.
#' @param a Positive real. Total mass of the centering measure.
#' @inheritParams gs3_log
#'
#' @keywords internal
#'
logacceptance_ratio_logu <- function(logu, logu_prime, n, r, gamma, kappa, a, delta) {
  log_ratio <- logf_logu_cond_y(logu_prime, n, r, gamma, kappa, a) - logf_logu_cond_y(logu, n, r, gamma, kappa, a) + logdprop_logu(logu, logu_prime, delta) - logdprop_logu(logu_prime, logu, delta)
  return(min(0, log_ratio))
}

#' Conditional posterior distribution of latent logU
#'
#' This function simulates from the conditional posterior distribution of a log transformation of the
#' latent U.
#'
#' @param logut Real, log of the latent variable U at the current iteration.
#' @param n Integer, number of data points.
#' @param r Integer, number of clusters.
#' @param alpha Positive real. Total mass of the centering measure.
#' @param kappa Positive real. A parameter of the NRMI process.
#' @param gama Real. \eqn{0\leq \texttt{gama} \leq 1}{0 <= gama <=
#' 1}.  See details.
#'
#' @param delta Scale of the Metropolis-Hastings proposal distribution
#'
#' @keywords internal
#'
gs3_log <-
  function(logut, n, r, alpha, kappa, gama, delta) {
    logu_prime <- rprop_logu(logu = logut, delta = delta)
    logq1 <- logacceptance_ratio_logu(logu = logut, logu_prime = logu_prime, n = n, r = r, gamma = gama, kappa = kappa, a = alpha, delta = delta)
    if (log(runif(n = 1)) < logq1) {
      return(logu_prime)
    } else {
      return(logut)
    }
  }


#' Conditional posterior distribution of latent U
#'
#' This function simulates from the conditional posterior distribution of the
#' latent U, with an adaptive proposal
#'
#' @keywords internal
#'
gs3_adaptive3 <- function(ut, n, r, alpha, kappa, gama, delta, U, iter, adapt = FALSE) {
  target_acc_rate <- 0.44
  batch_size <- 100
  if (adapt && (iter %% batch_size == 0)) {
    acc_rate <- length(unique(U[(iter - batch_size + 1):iter])) / batch_size
    logincrement <- 2 * min(0.25, 1 / sqrt(iter))
    # increment = min(0.5, 5 / sqrt(iter))
    if (acc_rate < 0.44) {
      delta_i <- delta * exp(-logincrement)
    } else {
      delta_i <- delta * exp(+logincrement)
    }
  } else {
    delta_i <- delta
  }
  logu_prime <- gs3_log(logut = log(ut), n = n, r = r, alpha = alpha, kappa = kappa, gama = gama, delta = delta_i)
  return(list(u_prime = exp(logu_prime), delta = delta_i))
}
