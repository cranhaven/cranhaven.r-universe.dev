#' @title Simulate Stock Price and Price of Underlying Asset
#'
#' @description At least one of \code{D}, \code{r}, or \code{T.} needs to have
#' the desired length of the simulated series. All vectors with length greater
#' than one needs to have the same length.
#'
#' @inheritParams BS_fit
#' @param vol numeric scalar with \eqn{\sigma} value.
#' @param mu numeric scalar with \eqn{\mu} value.
#' @param V_0 numeric scalar with starting value of the underlying asset, \eqn{S_{0}}.
#'
#' @examples
#' library(DtD)
#' set.seed(79156879)
#' sims <- BS_sim(
#'   vol = .1, mu = .05, dt = .2, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)
#'
#' # plot underlying
#' plot(sims$V)
#'
#' # plot stock
#' plot(sims$S)
#'
#' @seealso \code{\link{BS_fit}}
#'
#' @importFrom checkmate assert_number
#' @export
BS_sim <- function(vol, mu, dt, V_0, D, r, T.){
  #####
  # checks
  assert_number(vol, lower = 1e-16, finite = TRUE)
  assert_number(mu                , finite = TRUE)
  assert_number(dt , lower = 1e-16, finite = TRUE)
  assert_number(V_0, lower = 1e-16, finite = TRUE)

  .check_args(D = D, r = r, T. = T.)

  #####
  # get arguments for latter calls
  lens = c(length(D), length(T.), length(r), 1L)
  args <- .get_eq_length_args(
    lens = lens, D = D, T = T., r = r, vol = vol)

  #####
  # simulate and return
  time <- (1:max(lens) - 1L) * dt
  V <- V_0 * exp(
    (mu - vol^2/2) * time + cumsum(c(
      0, rnorm(length(time) - 1L, sd = vol * sqrt(dt)))))
  S <- with.default(args, BS_call(V, D, T. = T, r, vol))

  with.default(args, data.frame(V, S, time, D, r, vol, mu, T))
}
