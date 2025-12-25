#' Simulates event times from an inhomogenous Poisson process on [0,T]
#' 
#' @param targetfn A first order function defining the process intensity
#' @param maxintensity The maximum values of targetfn
#' @param T Length of the interval [0,T] on which to simulate the process
#' @return The simulated event times
#' @author Gordon J Ross
#' @examples
#' simulateNHPP(function(x) {sin(x)+1}, 2, 100)
#' simulateNHPP(function(x) {x^2}, 100, 10)
#' @export
simulateNHPP <- function(targetfn, maxintensity,T=Inf) {
  pts <- numeric()
  s <- 0
  while (TRUE) {
    s <- s + rexp(1,maxintensity)
    if (s > T) {break}
    if (runif(1) < targetfn(s)/maxintensity) {
      pts <- c(pts,s)
    }
  }
  return(pts)   
}