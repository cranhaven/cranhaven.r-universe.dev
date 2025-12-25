#' @title  Fit Black-Scholes Parameters Over Rolling Window
#'
#' @description
#' Function to estimate the volatility, \eqn{\sigma}, and drift, \eqn{\mu}. E.g.,
#' the window can be over a given number of months. See
#' \code{vignette("Distance-to-default", package = "DtD")} for details.
#'
#' @inheritParams BS_fit
#' @param grp integer vector with the group identifier (e.g., units of
#' months).
#' @param width integer scalar with the units of \code{grp} to include in the
#' rolling window.
#' @param min_obs integer scalar for the minimum number of observation required
#' in each window.
#'
#' @return
#' Matrix with the \code{grp}, number of observation in the window, parameter
#' estimates, and \code{'n_iter'} as in \code{\link{BS_fit}}, and whether the
#' estimation method was successful.
#'
#' An \code{error} attribute is added in case other code than
#' \code{\link{optim}} fails. It is a list of lists with the \code{grp} index
#' where the method failed and the output from \code{\link{try}}.
#'
#' @seealso
#' \code{\link{BS_fit}}
#'
#' @examples
#' # Simulate data
#' set.seed(55770945)
#' n <- 21L * 3L * 12L # 21 trading days for 3 years w/ 12 months
#' sims <- BS_sim(
#'   vol = .1, mu = .05, dt = .1, V_0 = 100, T. = 1,
#'   D = runif(n, 80, 90), r = runif(n, 0, .01))
#' sims$month <- (1:nrow(sims) - 1L) %/% 21L + 1L
#'
#' # throw out some months
#' sims <- subset(sims, !month %in% 15:24)
#'
#' # assign parameters
#' grp <- sims$month
#' width <- 12L        # window w/ 12 month width
#' min_obs <- 21L * 3L # require 3 months of data
#'
#' # estimate results with R loop which is slightly simpler then the
#' # implementation
#' grps <- unique(grp)
#' out <- matrix(
#'   NA_real_, nrow = length(grps), ncol = 6,
#'   dimnames = list(NULL, c("mu", "vol", "n_iter", "success", "n_obs", "grp")))
#' for(g in grps){
#'   idx <- which(grps == g)
#'   keep <- which(grp %in% (g - width + 1L):g)
#'   out[idx, c("n_obs", "grp")] <- c(length(keep), g)
#'   if(length(keep) < min_obs)
#'     next
#'   res <- with(
#'     sims[keep, ],
#'     BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "iterative",
#'            vol_start = 1))
#'   out[idx, c("mu", "vol", "n_iter", "success")] <- rep(
#'     do.call(c, res[c("ests", "n_iter", "success")]), each = length(idx))
#' }
#'
#' # we get the same with the R function
#' out_func <- with(sims, BS_fit_rolling(
#'   S = S, D = D, T. = T, r = r, time = time, method = "iterative",
#'   grp = month, width = width, min_obs = min_obs))
#'
#' all.equal(out[, names(out) != "n_iter"],
#'           out_func[, names(out_func) != "n_iter"])
#'
#' @importFrom checkmate assert_count assert_integer
#' @export
BS_fit_rolling <- function(
  S, D, T., r, time, dt, vol_start, method = c("iterative", "mle"),
  tol = 1e-12, eps = 1e-8, grp, width, min_obs){
  #####
  # checks
  assert_count(width  , positive = TRUE)
  assert_count(min_obs, positive = TRUE)

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

  assert_integer(grp, any.missing = FALSE, len = max(lens))

  #####
  # find output

  # use heuristic from Bharath et al. (2008)
  vol_start <- sd(diff(log(S))) * tail(S, 1) / (tail(S, 1) + tail(D, 1))

  args <- .get_eq_length_args(
    lens = lens, S = S, D = D, T = T., r = r, time = time)
  args <- as.data.frame(args) # to use `subset`

  grps <- unique(grp)
  out <- matrix(
    NA_real_, nrow = length(grps), ncol = 6,
    dimnames = list(NULL, c("mu", "vol", "n_iter", "success", "n_obs", "grp")))
  errs <- NULL
  for(g in grps){
    idx <- which(grps == g)
    keep <- which(grp %in% (g - width + 1L):g)
    out[idx, c("n_obs", "grp")] <- c(length(keep), g)
    if(length(keep) < min_obs)
      next
    res <- try(with.default(
      args[keep, ], BS_fit_cpp(S, D, T, r, time, vol_start, method, tol, eps)),
      silent = TRUE)

    if(inherits(res, "try-error")){
      out[idx, "success"] <- FALSE
      errs <- c(errs, list(list(grp = g, error = res)))
      next

    }

    out[idx, c("mu", "vol", "n_iter", "success")] <- rep(
      do.call(c, res[c("ests", "n_iter", "success")]), each = length(idx))
    if(res$success)
      vol_start <- res$ests["vol"]
  }

  if(length(errs) > 0)
    attr(out, "errors") <- errs

  out
}
