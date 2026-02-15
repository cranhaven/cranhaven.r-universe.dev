#' PH fitting with three moments
#' 
#' Estimates PH parameters from three moments.
#' 
#' @param m1 A value of the first moment.
#' @param m2 A value of the second moment.
#' @param m3 A value of the third moment.
#' @param method The name of moment matching method.
#' @param max.phase An integer for the maximum number of phases in the method
#' "Osogami06".
#' @param epsilon A value of precision in the method "Osogami06".
#' @return An object of GPH.
#' 
#' @note
#' The method "Osogami06" checks the first three moments on whether there
#' exists a PH whose three moments match to them. In such case, the method
#' "Bobbio05" often returns an error.
#' 
#' @references
#' Osogami, T. and Harchol-Balter, M. (2006)
#' Closed Form Solutions for Mapping General Distributions to Minimal PH Distributions.
#' \emph{Performance Evaluation}, \bold{63}(6), 524--552.
#' 
#' Bobbio, A., Horvath, A. and Telek, M. (2005)
#' Matching Three Moments with Minimal Acyclic Phase Type Distributions.
#' \emph{Stochastic Models}, \bold{21}(2-3), 303--326.
#' 
#' @examples
#' ## Three moment matching
#' ## Moments of Weibull(shape=2, scale=1); (0.886227, 1.0, 1.32934)
#' (result1 <- phfit.3mom(0.886227, 1.0, 1.32934))
#' 
#' ## Three moment matching
#' ## Moments of Weibull(shape=2, scale=1); (0.886227, 1.0, 1.32934)
#' (result2 <- phfit.3mom(0.886227, 1.0, 1.32934, method="Bobbio05"))
#' 
#' ## mean
#' ph.mean(result1)
#' ph.mean(result2)
#' 
#' ## variance
#' ph.var(result1)
#' ph.var(result2)
#' 
#' ## up to 5 moments 
#' ph.moment(5, result1)
#' ph.moment(5, result2)
#'
#' @export

phfit.3mom <- function(m1, m2, m3, method = c("Osogami06", "Bobbio05"),
	max.phase = 50, epsilon = sqrt(.Machine$double.eps)) {
  method <- match.arg(method)
  switch(
    method,
    "Osogami06" = {
      res <- matching3PH(c(m1, m2, m3), epsilon=epsilon, max.phase=max.phase)
      ph(alpha=res$tau, Q=res$T, xi=res$xi)
    },
    "Bobbio05" = {
      mm.bobbio05(m1, m2, m3)
    },
    stop("The method should be chosen from Osogami06 and Bobbio05.")
  )
}
