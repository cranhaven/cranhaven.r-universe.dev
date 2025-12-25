#' @title European Call Option Price and the Inverse
#'
#' @description Computes the European call option and the inverse. All vectors
#' with length greater than one needs to have the same length.
#'
#' @param V numeric vector or scalar with price of the underlying asset.
#' @param vol numeric vector or scalar with volatilities, \eqn{\sigma}s.
#' @inheritParams BS_fit
#'
#' @return
#' Numeric vector or scalar with price of the underlying asset or equity price.
#'
#' @examples
#' library(DtD)
#' set.seed(58661382)
#' sims <- BS_sim(
#'   vol = .2, mu = .03, dt = .1, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)
#'
#' stopifnot(with(
#'   sims, isTRUE(all.equal(V, get_underlying(S, D, T, r, vol)))))
#' stopifnot(with(
#'   sims, isTRUE(all.equal(S, BS_call(V, D, T, r, vol)))))
#'
#' @seealso \code{\link{BS_fit}}
#'
#' @export
get_underlying <- function(S, D, T., r, vol, tol = 1e-12){
  lens <- c(length(S), length(D), length(T.), length(r), length(vol))
  if(all(lens == 1)) # return quickly
    return(drop(get_underlying_cpp(
      S = S, D = D, T = T., r = r, vol = vol, tol = tol)))

  # make sure all args have same length and call cpp code
  .check_args(S = S, D = D, T. = T., r = r, vol = vol, tol = tol)
  args <- .get_eq_length_args(
    lens = lens, S = S, D = D, T = T., r = r, vol = vol)
  with.default(args, drop(get_underlying_cpp(
    S = S, D = D, T = T, r = r, vol = vol, tol)))
}
