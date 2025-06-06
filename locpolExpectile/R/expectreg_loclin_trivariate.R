#' @title Local linear expectile regression (iterative procedure) for
#' a trivariate covariate case
#'
#' @description Formula interface for the local linear expectile estimation
#' for a trivariate covariate setting.
#'
#' @param Z The first covariate data values.
#' @param X1 The second covariate data values.
#' @param X2 The third covariate data values.
#' @param Y The response data values.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param h Smoothing parameter, bandwidth.
#'
#' @return \code{\link{expectreg_loclin_trivariate}} local linear expectile estimator
#' proposed and studied by Adam and Gijbels (2021b) for a trivariate covariate matrix.
#' \code{\link{expectreg_loclin_trivariate}} returns a vector whose components are
#' the estimation of the nonparametric part according to the observed values
#' (i.e. \eqn{(Z_i,X_{1i},X_{2i})}).
#'
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @import matrixcalc
#' @rdname expectreg_loclin_trivariate
#'

#'
#' @references{
#'
#' Adam, C. and Gijbels, I. (2021b). Partially linear expectile regression using
#' local polynomial fitting. In Advances in Contemporary Statistics and Econometrics:
#' Festschrift in Honor of Christine Thomas-Agnan, Chapter 8, pages 139–160. Springer, New York.
#'
#' }
#'
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @import matrixcalc

#' @name expectreg_loclin_trivariate
#' @export


expectreg_loclin_trivariate<-function(Z,X1,X2,Y,omega,h,kernel=gaussK)
{
  GRIDZ=Z
  GRID_X1=X1
  GRID_X2=X2
  TAU<-array(0,dim=c(length(GRIDZ)))
  LAMBDA1<-array(0,dim=c(length(GRIDZ)))
  LAMBDA2<-array(0,dim=c(length(GRIDZ)))
  LAMBDA3<-array(0,dim=c(length(GRIDZ)))
  Grid=cbind(GRIDZ,GRID_X1,GRID_X2)

  #Initial values for a and b
  a_first=lm(Y~Z+X1+X2)$coefficient[1]
  b_first=lm(Y~Z+X1+X2)$coefficient[2]
  c_first=lm(Y~Z+X1+X2)$coefficient[3]
  d_first=lm(Y~Z+X1+X2)$coefficient[4]
  for(i in 1:length(GRIDZ))#gridZ
  {
    a=0
    b=0
    c=0
    d=0

    z=Grid[i,1]
    x1=Grid[i,2]
    x2=Grid[i,3]
    a=a_first
    b=b_first
    c=c_first
    d=d_first

    #Initial value : OLS
    closea = FALSE

    while(closea == FALSE)#do
    {
      weight<-NULL

      #Compute r_i(x,a,b)
      for(l in 1:length(Z))
      {
        if(Y[l]<=a+(b*(Z[l]-z))+(c*(X1[l]-x1))+(d*(X2[l]-x2)))
        {
          weight[l]=(1-omega)*(1/h^3)*kernel(((Z[l]-z)/h))*kernel(((X1[l]-x1)/h))*kernel(((X2[l]-x2)/h))
        }
        else
        {
          weight[l]=omega*(1/h^3)*kernel(((Z[l]-z)/h))*kernel(((X1[l]-x1)/h))*kernel(((X2[l]-x2)/h))
        }
      }


      W=diag(weight)
      D=cbind(1,Z-z,X1-x1,X2-x2)

      tau1<-((matrix.inverse(t(D)%*%W%*%D))%*%t(D)%*%W%*%Y)[1,]
      lambda1<-((matrix.inverse(t(D)%*%W%*%D))%*%t(D)%*%W%*%Y)[2,]
      lambda2<-((matrix.inverse(t(D)%*%W%*%D))%*%t(D)%*%W%*%Y)[3,]
      lambda3<-((matrix.inverse(t(D)%*%W%*%D))%*%t(D)%*%W%*%Y)[4,]
      #ginv from MASS
      #matrix.inverse

      closea<-abs(tau1-a)<1*10^-6
      a=tau1
      b=lambda1
      c=lambda2
      d=lambda3
    }

    TAU[i]=a
    LAMBDA1[i]=b
    LAMBDA2[i]=c
    LAMBDA3[i]=d
  }
  return(TAU)
}
