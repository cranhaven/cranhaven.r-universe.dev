#' @title Rule-of-Thumb bandwidth selectors in a location-scale setting
#' without using the one-to-one mapping
#'
#' @description Rule-of-Thumb bandwidth selector for the expectile regression
#' in a location scale without using the one-to-one mapping. The weight function
#' is chosen to be equal to the indicator function on \eqn{[min(X_i)+0.1,max(X_i)-0.1]}.
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

#' @return \code{\link{h_GenROT_without}} provides
#' the Rule-of-Thumb bandwidth selector in a location-scale setting without
#' using the one-to-one mapping for univariate expectile regression
#' proposed by Adam and Gijbels (2021a).
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname h_GenROT_without
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
#' h=h_GenROT_without(X=x,Y=y,j=0,p=1,kernel=gaussK,omega=0.3)
#' #h=1.937706
#'
#' @name h_GenROT_without
#' @export

h_GenROT_without<-function(X,Y,j=0,p=1,kernel=gaussK,omega)
{
  k_1=min(X)+0.1
  k_2=max(X)-0.1
  C_p_nu=locpol::cteNuK(nu=j,p=p,kernel=kernel)
  fd<-compDerEstError_exp(X=X, Y=Y, p=p, omega=omega)

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

  sum3=0
  for(i in 1:length(Y))
  {
    if(fd$err[i]<= fd$err_fit[i])
    {
      sum3=sum3+1
    }
    else
    {
      sum3=sum3+0
    }
  }

  sum4=0
  for(i in 1:length(Y))
  {
    if(fd$err[i]> fd$err_fit[i])
    {
      sum4=sum4+1
    }
    else
    {
      sum4=sum4+0
    }
  }
  gamma=(omega*(1/length(Y))*sum4)+((1-omega)*(1/length(Y))*sum3)

  numerateur=((k_2-k_1)*sum*(1/length(X)))
  Denominateur=sum_2*(1/length(X))*(gamma^2)
  h_optimal=C_p_nu*((numerateur/Denominateur)^(1/((2*p)+3)))*(length(X)^(-1/((2*p)+3)))
  return(h_optimal)
}

#' @return \code{\link{compDerEstError_exp}} returns a data frame whose components are:
#' \itemize{
#' \item \code{X} The covariate data values.
#' \item \code{Y} The response data values.
#' \item \code{fit} The fitted values for the parametric estimation.
#' \item \code{der} The derivative estimation at \eqn{X} values.
#' \item \code{err} The estimation of the error vector
#' \item \code{err_fit} The \eqn{\omega}th expectile of the residuals
#'  }
#'
#'
#' @import expectreg
#' @import locpol
#' @rdname h_GenROT_without
#' @export
compDerEstError_exp<-function (X, Y, p, omega)
{
  xnam <- paste("x^", 2:((p+1) + 3), sep = "")
  xnam <- paste("rb(", xnam, ",type='parametric')")
  fmla <- as.formula(paste("y ~ 1 + rb(X,type='parametric') + ", paste(xnam, collapse = "+")))
  lmFit <- expectreg::expectreg.ls(fmla, data = data.frame(X,Y),estimate="laws"
                                   ,smooth ="schall",expectiles=omega)
  cp <- cumprod(1:(p + 4))
  coef <- coefficients(lmFit)
  der <- (c(coef[[p +1]])*c(cp[[p+1]]))+(c(coef[[p+2]])*c(cp[[p+2]])*X)+(c(coef[[p+3]])*c(cp[[p+3]])*(X^2/2))+(c(coef[[p+4]])*c(cp[[p+4]])*(X^3/6))

  error=as.vector(Y-fitted(lmFit))
  fmerrorla <- as.formula(paste("error ~ 1 + rb(X,type='parametric') + ", paste(xnam, collapse = "+")))
  lmErrorFit=expectreg::expectreg.ls(fmerrorla, data = data.frame(X,error),estimate="laws"
                                        ,smooth ="schall",expectiles=omega)
  res <- data.frame(X, Y, fit = fitted(lmFit), der, err=error ,err_fit=fitted(lmErrorFit))
  return(res)
}

