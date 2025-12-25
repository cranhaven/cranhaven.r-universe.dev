#' @title  Fit Black-Scholes Parameters
#'
#' @description
#' Function to estimate the volatility, \eqn{\sigma}, and drift, \eqn{\mu}. See
#' \code{vignette("Distance-to-default", package = "DtD")} for details. All
#' vectors with length greater than one needs to have the same length. The
#' Nelder-Mead method from \code{\link{optim}} is used when
#' \code{method = "mle"}. Either \code{time} or \code{dt} should be passed.
#'
#' @param S numeric vector with observed stock prices.
#' @param D numeric vector or scalar with debt due in \code{T.}.
#' @param T. numeric vector or scalar with time to maturity.
#' @param r numeric vector or scalar with risk free rates.
#' @param time numeric vector with the observation times.
#' @param dt numeric scalar with time increments between observations.
#' @param vol_start numeric scalar with starting value for \eqn{\sigma}.
#' @param method string to specify which estimation method to use.
#' @param tol numeric scalar with tolerance to \code{\link{get_underlying}}.
#' The difference is scaled  if the absolute of \code{S} is large than \code{tol}
#' as in the \code{tolerance} argument to \code{\link{all.equal.numeric}}.
#' @param eps numeric scalar with convergence threshold.
#'
#' @return
#' A list with the following components
#' \item{ests}{estimates of \eqn{\sigma}, and drift, \eqn{\mu}.}
#' \item{n_iter}{number of iterations when \code{method = "iterative"}
#' and number of log likelihood evaluations when \code{method = "mle"}.}
#' \item{success}{logical for whether the estimation method converged.}
#'
#' @section Warning:
#' Choosing \code{tol >= eps} or roughly equal may make the method alternate
#' between two solutions for some data sets.
#'
#' @examples
#' library(DtD)
#' set.seed(83486778)
#' sims <- BS_sim(
#'   vol = .1, mu = .05, dt = .1, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)
#'
#' with(sims,
#'      BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle"))
#'
#' @importFrom checkmate assert_number
#' @importFrom stats rnorm sd
#' @importFrom utils tail
#' @export
BS_fit <- function(S, D, T., r, time, dt, vol_start,
                   method = c("iterative", "mle"), tol = 1e-12, eps = 1e-8){
  #####
  # checks
  method <- method[1]
  cl <- match.call()
  m <- match(c("S", "D", "T.", "r", "time", "dt", "method", "tol", "eps"),
             names(cl), 0L)
  cl <- cl[c(1L, m)]
  cl[c("method", "tol", "eps")] <- list(method, tol, eps)
  tmp <- new.env(parent = parent.frame())
  tmp$.BS_fit_check_n_setup <- .BS_fit_check_n_setup
  cl[[1L]] <- quote(.BS_fit_check_n_setup)
  out <- eval(cl, tmp)
  lens <- out$lens
  time <- out$time

  if(missing(vol_start)){
    # use heuristic from Bharath et al. (2008)
    vol_start <- sd(diff(log(S))) * tail(S, 1) / (tail(S, 1) + tail(D, 1))

  } else
    assert_number(vol_start, lower = 1e-16, finite = TRUE)

  #####
  # call cpp code and return
  args <- .get_eq_length_args(
    lens = lens, S = S, D = D, T = T., r = r, time = time)
  with.default(args, BS_fit_cpp(S, D, T, r, time, vol_start, method, tol, eps))
}

#' @importFrom checkmate assert_choice
.BS_fit_check_n_setup <- function(S, D, T., r, time, dt, method, tol, eps,
                                  is_missing_method_eps_ok = FALSE,
                                  min_len = 3L){
  if(!(is_missing_method_eps_ok && missing(method)))
    assert_choice(method, c("iterative", "mle"))
  if(!missing(time) && !missing(dt))
    stop("Either ", sQuote("dt"), " or ", sQuote("time"), " should be passed")

  if(missing(time) || is.null(time)){
    lens <- c(length(S), length(D), length(T.), length(r))
    max_len = max(max(lens))
    time <- (1:max_len - 1L) * dt
    lens <- c(lens, max_len)

  } else {
    lens <- c(length(S), length(D), length(T.), length(r), length(time))
    max_len = max(max(lens))

  }
  stopifnot(length(time) == max_len)
  stopifnot(max_len >= min_len)

  if(is_missing_method_eps_ok && missing(eps)){
    .check_args(
      S = S, D = D, T. = T., r = r, time = time, tol = tol)
  } else
    .check_args(
      S = S, D = D, T. = T., r = r, time = time, tol = tol, eps = eps)

  list(lens = lens, time = time) # used later
}
