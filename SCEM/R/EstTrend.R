#' @title Estimates the trend function for a time series.
#'
#' @description The trend function for each individual time series is estimated non-parametrically
#' by the local linear estimate (as discussed in Fan and Gijbels (1996)). Detailed description can
#' be found in Chazin et al. 2019, Supplemental Materials 1.
#'
#' @param y A vector of time series observations.
#'
#' @param time A vector of time points where the value of the trend needs to be estimated.
#'
#' @param bandwidth Denotes the order of the bandwidth that should be used in the estimation process.
#' bandwidth = k will mean that the bandwidth is n^k.
#'
#' @export
#'
#' @return A vector of estimated values for the trend function at the given time-points.
#'
#' @examples
#'  armenia_split = split(armenia,f = armenia$ID)
#'  band = -0.33
#'  z = armenia_split[[1]]$oxygen
#'  n = length(z)
#'  ndx = (1:n)/n
#'  EstTrend(z,ndx,band)

EstTrend <- function(y,
                     time,
                     bandwidth){

  if (! is.atomic(y) || is.list(y)) {stop('y is not a vector')}
  if (! is.atomic(time) || is.list(time)) {stop('time is not a vector')}
  if (! is.atomic(bandwidth) || !length(bandwidth)==1) {stop('bandwidth needs to be a single value')}
  if (any(is.na(y))) {stop('y has NA values')}

  n = length(y)
  bn = n^{bandwidth}
  x = 1:n
  out = numeric(length(time))
  for (i in 1:length(time)){
    t = time[i]
    S0 = sum(kernel((x/n-t)/bn))
    S1 = sum((t-x/n)*kernel((x/n-t)/bn))
    S2 = sum((t-x/n)^2*kernel((x/n-t)/bn))
    w = kernel((x/n-t)/bn)*(S2-(t-x/n)*S1)/(S2*S0-S1^2)
    out[i] = sum(y*w)
  }

  return(out)

}
