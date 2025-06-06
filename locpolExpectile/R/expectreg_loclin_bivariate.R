#' @title Local linear expectile regression (iterative procedure) for
#' a bivariate covariate case
#'
#' @description Formula interface for the local linear expectile estimation
#' for a bivariate covariate case.
#'
#' @param Z1 The first covariate data values.
#' @param Z2 The second covariate data values.
#' @param Y The response data values.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param h Smoothing parameter, bandwidth.
#' @param grid Matrix of evaluation points. In default setting, a grid of
#' equispaced grid-values on the domain of the variables \code{Z1} and \code{Z2}.

#' @return \code{\link{expectreg_loclin_bivariate}} local linear expectile estimator
#' proposed and studied by Adam and Gijbels (2021b) for a bivariate covariate case.
#' \code{\link{expectreg_loclin_bivariate}} returns a matrix whose components are
#' the estimation of the bivariate expectile surface, of order \eqn{\omega} according to the grid matrix.
#' The rows are the grid on the first covariate data values (i.e. \code{Z1})
#' and the columns the grid on the second covariate data values (i.e. \code{Z2}).
#'
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname expectreg_loclin_bivariate
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
#' @examples
#' library(locpol)
#' library(lestat)
#' set.seed(6)
#' dist <- muniformdistribution(rep(0, 2), rep(1, 2))
#' values<-simulate(dist,200)
#' Z_1<-values[,1]
#' Z_2<-values[,2]
#' Z<-rbind(Z_1,Z_2)
#' gamma=cbind(3,-0.4)
#' set.seed(7)
#' eta_1<-rnorm(100,0,1)
#' X1=(gamma%*%Z)+(1.5*eta_1)
#' set.seed(8)
#' eta_2<-rnorm(100,0,2)
#' X2=(gamma%*%Z)+(1.5*eta_2)
#' X<-rbind(X1,X2)
#' set.seed(9)
#' epsilon<-rt(100,3)
#' delta_true<-rbind(0,-0.8)
#' Y=as.numeric((t(delta_true)%*%X)+(0.2*exp(1.5*(gamma%*%Z)))+epsilon)
#'
#' expectreg_loclin_bivariate(Z1=Z_1,Z2=Z_2,Y=Y,omega=0.1
#' ,kernel=gaussK,h=0.1,grid=cbind(seq(min(Z_1),max(Z_1)
#' ,length.out=10),seq(min(Z_2),max(Z_2),length.out=10)))
#'
#' @name expectreg_loclin_bivariate
#' @export


expectreg_loclin_bivariate<-function(Z1,Z2,Y,omega,kernel=gaussK,h,grid=cbind(seq(min(Z1),max(Z1),length.out=length(Z1)),seq(min(Z2),max(Z2),length.out=length(Z2))))
{

  TAU<-array(0,dim=c(length(grid[,1]),length(grid[,2])))
  LAMBDA1<-array(0,dim=c(length(grid[,1]),length(grid[,2])))
  LAMBDA2<-array(0,dim=c(length(grid[,1]),length(grid[,2])))


  #Initial values for a and b
  a_first=stats::lm(Y~Z1+Z2)$coefficient[1]
  b_first=stats::lm(Y~Z1+Z2)$coefficient[2]
  c_first=stats::lm(Y~Z1+Z2)$coefficient[3]
  for(i in 1:length(grid[,1]))#grid1
  {
    print(i)
    a=0
    b=0
    c=0

    z1=grid[i,1]
    a=a_first
    b=b_first
    c=c_first
    for(k in 1:length(grid[,2]))#grid2
    {
      z2=grid[k,2]
      z=cbind(z1,z2)
      #Initial value : OLS
      closea = FALSE
      closeb = FALSE
      closec = FALSE
      while(closea == FALSE )#do
      {
        weight<-NULL

        #Compute r_i(x,a,b)
        for(l in 1:length(Z1))
        {
          if(Y[l]<=a+(b*(Z1[l]-z[1]))+(c*(Z2[l]-z[2])))
          {
            weight[l]=(1-omega)*kernel(((Z1[l]-z[1])/h))*kernel(((Z2[l]-z[2])/h))
          }
          else
          {
            weight[l]=omega*kernel(((Z1[l]-z[1])/h))*kernel(((Z2[l]-z[2])/h))
          }
        }
        W=diag(weight)
        Z_D=cbind(1,Z1-z1,Z2-z2)

        tau<-((matrixcalc::matrix.inverse(t(Z_D)%*%W%*%Z_D))%*%t(Z_D)%*%W%*%Y)[1,]
        lambda1<-((matrixcalc::matrix.inverse(t(Z_D)%*%W%*%Z_D))%*%t(Z_D)%*%W%*%Y)[2,]
        lambda2<-((matrixcalc::matrix.inverse(t(Z_D)%*%W%*%Z_D))%*%t(Z_D)%*%W%*%Y)[3,]


        closea<-abs(tau-a)<1*10^-6
        a=tau
        b=lambda1
        c=lambda2
      }

      TAU[i,k]=a
      LAMBDA1[i,k]=b
      LAMBDA2[i,k]=c
    }
  }
  return(TAU)
}

