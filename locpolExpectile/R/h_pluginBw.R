#' @title Quantile-based bandwidth selectors based on the plug-in bandwidth
#' selector for mean regression
#'
#' @description Quantile-based bandwidth selector for univariate expectile regression
#' based on the plug-in bandwidth selector proposed by Fan and Gijbels
#' (1996).
#'
#' @param X The covariate data values.
#' @param Y The response data values.
#' @param p The order of the local polynomial estimator. In default setting,
#' \code{p=1}.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @return \code{\link{h_pluginBw}} provides the quantile-based bandwidth
#' based on the plugin bandwidth selector for mean regression proposed by
#' Fan and Gijbels (1996) as discussed in Adam and Gijbels (2021a).
#' @import locpol
#' @import stats
#' @rdname h_pluginBw
#'
#'
#'
#' @references{
#' Fan, J. and Gijbels, I. (1996). Local Polynomial Modelling and Its
#' Applications. Number 66 in Monographs on statistics and applied probability
#' series. Chapman and Hall, London.
#'
#' Adam, C. and Gijbels, I. (2021a). Local polynomial expectile regression.
#' Annals of the Institute of Statistical Mathematics doi:10.1007/s10463-021-00799-y.
#'
#' }
#'

#'
#' @import locpol
#' @import stats
#' @examples
#' library(locpol)
#' data(mcycle)
#' y=mcycle$accel
#' x=mcycle$times
#'
#' h=h_pluginBw(X=x,Y=y,p=1,kernel=gaussK,omega=0.1)
#' #h=0.8602156
#'
#' @name h_pluginBw
#' @export


h_pluginBw<-function(X,Y,p=1,kernel=gaussK,omega)
{
  weights<-NULL
  for(i in 1:length(X))
  {
    if(X[i]<=(max(X)-0.1) & X[i]>=(min(X)+0.1))
    {
      weights[i]=1
    }
    else{
      weights[i]=0
    }
  }
  h_mean=locpol::pluginBw(X, Y, deg=p , kernel=kernel,weig=weights)
  h_opt=h_mean*(((omega*(1-omega))/((stats::dnorm(stats::qnorm(omega,0,1),0,1))^2))
                ^(1/((2*p)+3)))
  return(h_opt)
}
