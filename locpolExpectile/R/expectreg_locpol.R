#' @title Local polynomial expectile regression (iterative procedure), univariate covariate
#'
#' @description Formula interface for the local polynomial expectile estimation.
#'
#' @param X The covariate data values.
#' @param Y The response data values.
#' @param j The order of derivative of the expectile to be estimated. In default setting, \code{j=0}
#' (i.e. estimating the expectile regression function).
#' @param p The order of the local polynomial estimator. In default setting,
#' \code{p=1} (i.e. local linear estimator).
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param h Smoothing parameter, bandwidth.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param starting_value Method for the starting point. Choice between the
#' estimated (unconditional) mean, median and omega-quantile.
#' @param grid Vector of evaluation points. In default setting, a grid of 100
#' equispaced grid-values on the domain of the variable \eqn{X}.

#' @return \code{\link{expectreg_locpol}} local polynomial expectile estimator
#' proposed and studied by Adam and Gijbels (2021a).
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname expectreg_locpol
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
#' @import quantreg
#' @examples
#' library(locpol)
#' data(mcycle)
#' y=mcycle$accel
#' x=mcycle$times
#'
#' expectreg_locpol(X=x,Y=y,omega=0.3,h=0.4,kernel=gaussK,starting_value="mean"
#' ,grid=seq(min(x),max(x),length.out=10))
#'
#' @name expectreg_locpol
#' @export

expectreg_locpol<-function( X , Y , j=0 , p=1 , omega , h , kernel=gaussK , starting_value=c("mean", "median", "omega-quantile") , grid=seq(min(X),max(X),length.out=100) )
{
  #X: a vector of dimension n (dependent variable)
  #Y: a vector of dimension n (independent variable)
  #x: grid
  #omega: the value w (weight)
  #h: bandwidth value
  #a: is the value of the intercept in the equation a+bx=y (local linear regression)
  #b: is the value of the coefficient in the equation a+bx=y (local linear regression)

  Estimates<-matrix(0, nrow = length(grid), ncol = p+1)

  if(starting_value=="mean")
  {
    #Initial values for beta's
    #a_first=lm(Y~X)$coefficient[1]
    #b_first=lm(Y~X)$coefficient[2]
    for(i in 1:length(grid))#grid
    {
      x=grid[i]
      Xprim=(X-x)
      if(p==1)
      {
        fmla <- as.formula(paste("Y ~ 1 + Xprim "))
      }else
      {
        xnam <- paste("I(Xprim^", 2:p, sep = "")
        fmla <- as.formula(paste("Y ~ 1 + Xprim + ", paste(xnam,")", collapse = "+")))
      }

      lmFit <- stats::lm(fmla, data = data.frame(X,Y))
      coef <- coefficients(lmFit)
      a_itt=coef[1]
      #Initial value : OLS
      closea = FALSE

      while(closea == FALSE)#do
      {

        #Compute r_i(x)
        weight<-NULL
        for(s in 1:length(X))
        {
          if(Y[s]<=lmFit$fitted.values[s])
          {
            weight[s]=(1-omega)*kernel((X[s]-x)/h)
          }
          else
          {
            weight[s]=omega*kernel((X[s]-x)/h)
          }
        }

        X_D<-matrix(1, nrow = length(X), ncol = p+1)
        S_n<-matrix(0, nrow = p+1, ncol = p+1)
        for(l in 2:(p+1))
        {
          X_D[,l]<-Xprim^(l-1)
        }

        WH<-diag(weight)

        S_n<-t(X_D)%*%(WH%*%X_D)

        lmFit$coefficients<-(solve(S_n)%*%t(X_D)%*%WH%*%Y)[,1]
        lmFit$fitted.values<-predict(lmFit)

        closea<-abs(lmFit$coefficients[1]-a_itt)<1*10^-6
        a_itt=lmFit$coefficients[1]
      }
      Estimates[i,]=lmFit$coefficients
    }
  }
  if(starting_value=="median")
  {
    {
      #Initial values for beta's
      #a_first=lm(Y~X)$coefficient[1]
      #b_first=lm(Y~X)$coefficient[2]
      for(i in 1:length(grid))#grid
      {
        x=grid[i]
        Xprim=(X-x)
        if(p==1)
        {
          fmla <- as.formula(paste("Y ~ 1 + Xprim "))
        }else
        {
          xnam <- paste("I(Xprim^", 2:p, sep = "")
          fmla <- as.formula(paste("Y ~ 1 + Xprim + ", paste(xnam,")", collapse = "+")))
        }

        lmFit <- quantreg::rq(fmla, data = data.frame(X,Y))
        coef <- coefficients(lmFit)
        a_itt=coef[1]
        #Initial value : OLS
        closea = FALSE

        while(closea == FALSE)#do
        {

          #Compute r_i(x)
          weight<-NULL
          for(s in 1:length(X))
          {
            if(Y[s]<=lmFit$fitted.values[s])
            {
              weight[s]=(1-omega)*kernel((X[s]-x)/h)
            }
            else
            {
              weight[s]=omega*kernel((X[s]-x)/h)
            }
          }

          X_D<-matrix(1, nrow = length(X), ncol = p+1)
          S_n<-matrix(0, nrow = p+1, ncol = p+1)
          for(l in 2:(p+1))
          {
            X_D[,l]<-Xprim^(l-1)
          }

          WH<-diag(weight)

          S_n<-t(X_D)%*%(WH%*%X_D)

          lmFit$coefficients<-(solve(S_n)%*%t(X_D)%*%WH%*%Y)[,1]
          lmFit$fitted.values <- X_D %*% lmFit$coefficients

          closea<-abs(lmFit$coefficients[1]-a_itt)<1*10^-6
          a_itt=lmFit$coefficients[1]
        }
        Estimates[i,]=lmFit$coefficients

      }
    }
  }
  if(starting_value=="omega_quantile")
  {
    {
      #Initial values for beta's
      #a_first=lm(Y~X)$coefficient[1]
      #b_first=lm(Y~X)$coefficient[2]
      for(i in 1:length(grid))#grid
      {
        x=grid[i]
        Xprim=(X-x)
        if(p==1)
        {
          fmla <- as.formula(paste("Y ~ 1 + Xprim "))
        }else
        {
          xnam <- paste("I(Xprim^", 2:p, sep = "")
          fmla <- as.formula(paste("Y ~ 1 + Xprim + ", paste(xnam,")", collapse = "+")))
        }

        lmFit <- quantreg::rq(fmla, data = data.frame(X,Y),tau=omega)
        coef <- coefficients(lmFit)
        a_itt=coef[1]
        #Initial value : OLS
        closea = FALSE

        while(closea == FALSE)#do
        {

          #Compute r_i(x)
          weight<-NULL
          for(s in 1:length(X))
          {
            if(Y[s]<=lmFit$fitted.values[s])
            {
              weight[s]=(1-omega)*kernel((X[s]-x)/h)
            }
            else
            {
              weight[s]=omega*kernel((X[s]-x)/h)
            }
          }

          X_D<-matrix(1, nrow = length(X), ncol = p+1)
          S_n<-matrix(0, nrow = p+1, ncol = p+1)
          for(l in 2:(p+1))
          {
            X_D[,l]<-Xprim^(l-1)
          }

          WH<-diag(weight)

          S_n<-t(X_D)%*%(WH%*%X_D)

          lmFit$coefficients<-(solve(S_n)%*%t(X_D)%*%WH%*%Y)[,1]
          lmFit$fitted.values <- X_D %*% lmFit$coefficients

          closea<-abs(lmFit$coefficients[1]-a_itt)<1*10^-6
          a_itt=lmFit$coefficients[1]
        }
        Estimates[i,]=lmFit$coefficients

      }
    }
  }
  return(Estimates)
}
