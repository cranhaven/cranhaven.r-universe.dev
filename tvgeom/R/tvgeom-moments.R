#' Moments for the The Time-Varying (Right-Truncated) Geometric Distribution
#'
#' Functions to calculate first moment \code{tvgeom_mean()} and second central
#' moment \code{tvgeom_var()} for the time-varying geometric distribution.
#'
#' @param prob vector of the probability of success for each trial/time step.
#' @return \code{tvgeom_mean} returns the moment (the mean), and
#' \code{tvgeom_var} returns the second central moment (the variance).
#' @name tvgeom-moments
#' @examples
#' tvgeom_mean(prob = rep(0.1, 5))
#' tvgeom_var(prob = rep(0.1, 5))
#' @rdname tvgeom-moments
#' @export
tvgeom_mean <- function(prob) {
  sum(dtvgeom(x = 1:(length(prob) + 1), prob = prob) * c(1:(length(prob) + 1)))
}

#' @rdname tvgeom-moments
#' @export
tvgeom_var <- function(prob) {
  sum(dtvgeom(x = 1:(length(prob) + 1), prob = prob) *
        (c(1:(length(prob) + 1))^2)) - tvgeom_mean(prob)^2
}
