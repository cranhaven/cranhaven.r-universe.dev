#' @title Rule-of-Thumb bandwidth selectors in a location-scale setting
#' using the one-to-one mapping
#'
#' @description Rule-of-Thumb bandwidth selector for the expectile regression
#' in a location-scale setting using the one-to-one mapping. The weight function is chosen
#' to be equal to the indicator function on \eqn{[min(X_i)+0.1,max(X_i)-0.1]}.
#'
#' @param X The covariate data values.
#' @param Y The response data values.
#' @param j The order of derivative to estimate. In default setting, \code{j=0}.
#' @param p The order of the local polynomial estimator. In default setting,
#' \code{p=1}.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.

#' @return \code{\link{h_GenROT_with}} provides the Rule-of-Thumb bandwidth
#' selector in a location-scale setting using the one-to-one mapping for
#' univariate expectile regression proposed by Adam and Gijbels (2021a).
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname h_GenROT_with
#'
#'
#'
#' @references{
#'
#' Adam, C. and Gijbels, I. (2021a). Local polynomial expectile regression.
#' Annals of the Institute of Statistical Mathematics doi:10.1007/s10463-021-00799-y.
#'
#' }
#'

#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @examples
#' library(locpol)
#' data(mcycle)
#' y=mcycle$accel
#' x=mcycle$times
#'
#' h=h_GenROT_with(X=x,Y=y,j=0,p=1,kernel=gaussK,omega=0.1)
#' #h=1.886125
#'
#' @name h_GenROT_with
#' @export

h_GenROT_with<-function(X,Y,j=0,p=1,kernel=gaussK,omega)
{
  k_1=min(X)+0.1
  k_2=max(X)-0.1
  C_p_nu=locpol::cteNuK(nu=j,p=p,kernel=kernel)
  fd<-compDerEstError_exp(X=X, Y=Y, p=p, omega=omega)
  alpha_esti<-uniroot(findroot,X=X,Y=Y,omega=omega,lower = 0.01, upper = 0.99)$root

  sum=0
  for(i in 1:length(Y))
  {
    s=0
    if(Y[i]<=fd$fit[i])
    {
      s=((1-omega)*(Y[i]-fd$fit[i]))^2
    }else
    {
      s=((omega)*(Y[i]-fd$fit[i]))^2
    }
    sum=sum+s
  }

  sum_2=0
  for(i in 1:length(X))
  {
    s_2=0
    if(k_1<= X[i] && X[i]<=k_2)
    {
      s_2=(fd$der[i])^2
    }else
    {
      s_2=0
    }
    sum_2=sum_2+s_2
  }

  #gamma
  gamma=((omega)+alpha_esti-(2*omega*alpha_esti))

  numerateur=((k_2-k_1)*sum*(1/length(X)))
  Denominateur=sum_2*(1/length(X))*(gamma^2)
  h_optimal=C_p_nu*((numerateur/Denominateur)^(1/((2*p)+3)))*(length(X)^(-1/((2*p)+3)))
  return(h_optimal)
}
