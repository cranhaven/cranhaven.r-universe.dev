#' Generate an affine error model.
#'
#' @param nobs The data length to be generated.
#' @param a intercept
#' @param b slope
#' @param ndim The number of potential predictors (default is 9).
#' @param mu mean of error term
#' @param sd standard deviation of error term
#'
#' @return A list of 2 elements: a vector of response (x), and a matrix of potential predictors (dp) with each column containing one potential predictor.
#' @export
#'
#' @references McColl, K. A., Vogelzang, J., Konings, A. G., Entekhabi, D., Piles, M., & Stoffelen, A. (2014). Extended triple collocation: Estimating errors and correlation coefficients with respect to an unknown target. Geophysical Research Letters, 41(17), 6229-6236. doi:10.1002/2014gl061322
#'
#' @examples
#' # Affine error model from paper with 3 dummy variables
#' data.affine<-data.gen.affine(500)
#' plot.ts(cbind(data.affine$x,data.affine$dp))

data.gen.affine<-function(nobs,a=0,b=1,ndim=3,mu=0,sd=1)
{
  x <-matrix(0,nobs,1)
  dp<-matrix(0,nobs,ndim)

  x<- runif(nobs)
  for(i in 1:ndim) dp[,i]<- a + b*x + rnorm(nobs,mean=mu,sd=sd)

  data_generated<-list(x=x,dp=dp)

  return(data_generated)
}


