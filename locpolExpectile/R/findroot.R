#' @title One-to-one mapping relating expectiles and quantiles
#'
#' @description Ont-to-one mapping (see Proposition 1 in Adam and Gijbels (2021a))
#'
#' @param X The covariate data values.
#' @param Y The response data values.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param alpha Alpha value corresponding to the omega value given by the
#' one-to-one mapping
#' @param p The order of the local polynomial estimator. In default setting,
#' \code{p=1}.
#'
#' @return \code{\link{findroot}} returns the one-to-one mapping
#'
#' @import expectreg
#' @import locpol
#' @rdname findroot
#'
#' @references{
#'
#' Adam, C. and Gijbels, I. (2021a). Local polynomial expectile regression.
#' Annals of the Institute of Statistical Mathematics doi:10.1007/s10463-021-00799-y.
#'
#' }
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @name findroot
#' @export
findroot<-function(X,Y,omega,alpha,p=1)
{
  fd<-compDerEstError_exp(X=X, Y=Y, p=p, omega=omega)
  fct<-NULL

  sum=0
  for(i in 1:length(X))
  {
    if(fd$err[i]-quantile(fd$err,probs=alpha,names=FALSE,type=8)<= fd$err_fit[i])
    {
      sum=sum+(fd$err[i])
    }
    else
    {
      sum=sum+0
    }
  }

  sum_2=0
  for(i in 1:length(fd$err))
  {
    if(fd$err[i]-quantile(fd$err,alpha,names=FALSE,type=8)> fd$err_fit[i])
    {
      sum_2=sum_2+(fd$err[i])
    }
    else
    {
      sum_2=sum_2+0
    }
  }

  numerator=(quantile(fd$err,alpha,names=FALSE,type=8)*alpha)-(sum/length(fd$err))
  denominator=(2*(sum_2/length(fd$err)))-(quantile(fd$err,alpha,names=FALSE,type=8)*(1-(2*alpha)))-(sum(fd$err)/length(fd$err))
  fct=(omega-(numerator/denominator))
  return(fct)
}


