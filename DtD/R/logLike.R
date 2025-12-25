#' @title Compute Log-Likelihood of Merton Model
#'
#' @description
#' Computes the log-likelihood for a given values of \eqn{\mu} and
#' \eqn{\sigma}.
#'
#' @inheritParams BS_fit
#' @param vol numeric scalar with the \eqn{\sigma} value.
#' @param mu numeric scalar with the \eqn{\mu} value.
#'
#' @seealso \code{\link{BS_fit}}
#'
#' @examples
#' # we get the same if we call `optim` as follows. The former is faster and is
#' # recommended
#' set.seed(4648394)
#' sims <- BS_sim(
#'   vol = .1, mu = .05, dt = .1, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)
#'
#' r1 <- with(
#'   sims, BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle",
#'                eps = 1e-8, vol_start = .2))
#'
#' r2 <- optim(c(mu = 0, log_vol = log(.2)), function(par)
#'   -with(
#'     sims, merton_ll(S = S, D = D, T. = T, r = r, time = time,
#'                     mu = par["mu"], vol = exp(par["log_vol"]))))
#'
#' all.equal(r1$n_iter, unname(r2$counts[1]))
#' all.equal(r1$ests[1], r2$par[1])
#' all.equal(r1$ests[2], exp(r2$par[2]), check.attributes = FALSE)
#'
#' # the log-likelihood integrates to one as it should though likely not the
#' # most stable way to test this
#' ll <- integrate(
#'   function(x) sapply(x, function(S)
#'     exp(merton_ll(
#'       S = c(1, S), D = .8, T. = 3, r = .01, dt = 1/250, vol = .2,
#'       mu = .05))),
#'   lower = 1e-4, upper = 6)
#' stopifnot(isTRUE(all.equal(ll$value, 1, tolerance = 1e-5)))
#'
#' @importFrom checkmate assert_number
#'
#' @export
merton_ll <- function(S, D, T., r, time, dt, vol, mu, tol = 1e-12){
  #####
  # checks
  cl <- match.call()
  m <- match(c("S", "D", "T.", "r", "time", "dt", "tol"), names(cl), 0L)
  cl <- cl[c(1L, m)]
  cl[c("tol", "is_missing_method_eps_ok")] <- list(tol, TRUE)
  tmp <- new.env(parent = parent.frame())
  tmp$.BS_fit_check_n_setup <- .BS_fit_check_n_setup
  cl[[1L]] <- quote(.BS_fit_check_n_setup)
  cl$min_len <- 2L
  out <- eval(cl, tmp)
  lens <- out$lens
  time <- out$time

  assert_number(
    vol, finite = TRUE, null.ok = FALSE, na.ok = FALSE, lower = 1e-16)
  assert_number(
    mu , finite = TRUE, null.ok = FALSE, na.ok = FALSE)

  #####
  # compute log likelihood
  args <- .get_eq_length_args(
    lens = lens, S = S, D = D, T = T., r = r, time = time)
  with.default(args, merton_ll_cpp(
    S = S, D = D, T = T, r = r, time = time, vol = vol, mu = mu, tol = tol))
}
