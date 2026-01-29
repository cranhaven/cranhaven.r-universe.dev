#' Quantile function truncated normal
#'
#' Computes the quantiles.
#'
#' For internal use
#'
#' @note Taken from \code{msm} R-package.
#' @author C. H. Jackson
#' @references Taken from
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(p, mean = 0, sd = 1, lower = -Inf, upper = Inf, lower.tail = TRUE,
#'          log.p = FALSE) {
#'   qgeneric(ptnorm,
#'     p = p, mean = mean, sd = sd, lower = lower,
#'     upper = upper, lbound = lower, ubound = upper, lower.tail = lower.tail,
#'     log.p = log.p
#'   )
#' }
qtnorm <-
  function(p, mean = 0, sd = 1, lower = -Inf, upper = Inf, lower.tail = TRUE,
           log.p = FALSE) {
    qgeneric(ptnorm,
      p = p, mean = mean, sd = sd, lower = lower,
      upper = upper, lbound = lower, ubound = upper, lower.tail = lower.tail,
      log.p = log.p
    )
  }
