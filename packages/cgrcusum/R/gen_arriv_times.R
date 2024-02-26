#' Generate arrival times according to a Poisson point process
#'
#' @description This function can be used to generate arrival times for a Poisson point
#' process with rate psi up until time t.
#'
#' @details Exponential(\eqn{\psi}) interarrival times.
#'
#' @param psi rate of the arrival process.
#' @param t time until which arrivals should be generated.
#'
#' @return A vector of arrival times up until time t.
#'
#' @export
#'
#' @author Daniel Gomon
#' @family utils
#' @importFrom stats rexp
#' @importFrom utils tail
#' @examples
#' gen_arriv_times(psi = 0.3, t = 5)


gen_arriv_times <- function(psi, t){
  #Generate first arrival after time 0
  parriv <- rexp(1, psi)
  i <- 1
  #While arrivals have not surpassed time t, generate more arrivals
  while(tail(parriv, 1) < t){
    extime <- rexp(1,psi)
    if(parriv[i] + extime <= t){
      parriv[i+1] <- parriv[i] + extime
    } else{break}
    i <- i + 1
  }
  parriv
}
