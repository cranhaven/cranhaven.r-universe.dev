#' Generate predictor and response data: Sinusoidal model
#'
#' @param nobs    The data length to be generated.
#' @param freq    The frequencies in the generated response. Default freq=50.
#' @param A       The amplitude of the sinusoidal series
#' @param phi     The phase of the sinusoidal series
#' @param mu      The mean of Gaussian noise in the variable.
#' @param sd      The standard deviation of Gaussian noise in the variable.
#'
#' @export
#' @return A list of time and x.
#'
#' @references Shumway, R. H., & Stoffer, D. S. (2011). Characteristics of Time Series. In D. S. Stoffer (Ed.), Time series analysis and its applications (pp. 8-14). New York : Springer.
#'
#' @examples
#' ### Sinusoidal model
#' delta <- 1/12 # sampling rate, assuming monthly
#' period.max<- 2^5
#'
#' N = 6*period.max/delta
#' scales<- 2^(0:5)[c(2,6)] #pick two scales
#' scales
#'
#' ### scale, period, and frequency
#' # freq=1/T; T=s/delta so freq = delta/s
#'
#' tmp <- NULL
#' for(s in scales){
#'   tmp <- cbind(tmp, data.gen.SW(nobs=N, freq = delta/s, A = 1, phi = 0, mu=0, sd = 0)$x)
#' }
#' x <- rowSums(data.frame(tmp))
#' plot.ts(cbind(tmp,x), type = 'l', main=NA)

data.gen.SW<-function(nobs=500,freq=50,A=2,phi=pi,mu=0,sd=1)
{

  t <- 0:(nobs-1)

  x <- A*cos(2*pi*freq*t + phi) + rnorm(nobs,mean=mu,sd=sd)

  return(list(t=t, x=x))
}

