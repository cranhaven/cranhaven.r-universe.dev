#' Density truncated normal
#'
#' Computes the density.
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
#' function(x, mean = 0, sd = 1, lower = -Inf, upper = Inf, log = FALSE) {
#'   ret <- numeric(length(x))
#'   ret[x < lower | x > upper] <- if (log) {
#'     -Inf
#'   } else {
#'     0
#'   }
#'   ret[upper < lower] <- NaN
#'   ind <- x >= lower & x <= upper
#'   if (any(ind)) {
#'     denom <- pnorm(upper, mean, sd) - pnorm(
#'       lower, mean,
#'       sd
#'     )
#'     xtmp <- dnorm(x, mean, sd, log)
#'     if (log) {
#'       xtmp <- xtmp - log(denom)
#'     } else {
#'       xtmp <- xtmp / denom
#'     }
#'     ret[x >= lower & x <= upper] <- xtmp[ind]
#'   }
#'   ret
#' }
dtnorm <-
  function(x, mean = 0, sd = 1, lower = -Inf, upper = Inf, log = FALSE) {
    ret <- numeric(length(x))
    ret[x < lower | x > upper] <- if (log) {
      -Inf
    } else {
      0
    }
    ret[upper < lower] <- NaN
    ind <- x >= lower & x <= upper
    if (any(ind)) {
      denom <- pnorm(upper, mean, sd) - pnorm(
        lower, mean,
        sd
      )
      xtmp <- dnorm(x, mean, sd, log)
      if (log) {
        xtmp <- xtmp - log(denom)
      } else {
        xtmp <- xtmp / denom
      }
      ret[x >= lower & x <= upper] <- xtmp[ind]
    }
    ret
  }
