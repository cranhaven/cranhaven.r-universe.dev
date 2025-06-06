#' @title Partially linear expectile regression with a homoscedastic error and
#' a bivariate variable in the nonparametric function
#'
#' @description Formula interface for the partially linear expectile regression
#' using local linear expectile estimation for a homoscedastic error and
#' a bivariate variable in the nonparametric function. For the nonparametric part,
#' the general Rule-of-Thumb bandwidth selector proposed in Adam and Gijbels (2021b)
#' is used. See Adam and Gijbels (2021b) for more details.
#'
#' @param X The covariates data values for the linear part (of size \eqn{n \times k}).
#' @param Y The response data values.
#' @param Z The covariates data values for the nonparametric part (of size \eqn{n \times 2}).
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param grid Matrix of evaluation points used for the nonparametric part. In default setting, a grid of 10
#' equispaced grid-values in each direction on the domain of the variable \code{Z}.
#'
#' @return \code{\link{ParLin_expectreg_homo_biv}} partially linear expectile estimators
#' assuming a homoscedastic error and a bivariate covariate in the nonparametric part,
#' proposed and studied by Adam and Gijbels (2021b). \code{\link{ParLin_expectreg_homo_biv}}
#' returns a list whose components are:
#' \itemize{
#'  \item \code{Linear} The delta estimators for the linear part
#'  \item \code{Nonlinear} The estimation of the nonparametric part
#'  according to the grid. The rows of the estimation matrix
#'  are the grid on the first covariate data values (i.e. \code{Z[,1]})
#'  and the columns the grid on the second covariate data values (i.e. \code{Z[,2]}).
#' }
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname ParLin_expectreg_homo_biv
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
#' ParLin_expectreg_homo_biv(X=t(X),Y=Y,Z=t(Z),omega=0.1,kernel=gaussK
#' ,grid=cbind(seq(min(Z[,1]),max(Z[,1]),length.out=10),seq(min(Z[,2]),max(Z[,2]),length.out=10)))
#'
#'
#' @name ParLin_expectreg_homo_biv
#' @export
#'

ParLin_expectreg_homo_biv<-function(X,Y,Z,omega=0.3,kernel=gaussK,grid=cbind(seq(min(Z[,1]),max(Z[,1]),length.out=10),seq(min(Z[,2]),max(Z[,2]),length.out=10)))
{
  #Order according to Z1
  Values=cbind(X,Y,Z)
  Values=Values[order(Values[,ncol(Values)-1]),]
  m_X_plug<-NULL
  #E[X|Z=Z_i]
  if(NCOL(X)==1)
  {
    for(i in 1:NCOL(X))
    {
      reg_locpol_plug<-NULL
      data<-data.frame(x=Values[,i])
      data$z1<-Values[,ncol(Values)-1]
      data$z2<-Values[,ncol(Values)]
      x__1=data$z1
      x__2=data$z2
      h_1<-locpol::pluginBw(data$z1,data$x,deg=1,kernel=kernel)
      h_2<-locpol::pluginBw(data$z2,data$x,deg=1,kernel=kernel)

      for(i in 1:length(x__1))
      {
        weight1<-NULL
        weight2<-NULL
        X_tilt_1=data$z1-x__1[i]
        X_tilt_2=data$z2-x__2[i]
        #Compute weight
        for(j in 1:length(data$z2))
        {
          weight1[j]=kernel((X_tilt_1[j]/h_1))
          weight2[j]=kernel((X_tilt_2[j]/h_2))
        }
        reg_locpol_plug[i]=stats::lsfit(cbind(X_tilt_1,X_tilt_2),data$x,weight1*weight2)$coef[1]
      }
      #vector m_X(z_i)
      m_X_plug=cbind(m_X_plug,reg_locpol_plug)
    }

    #Estimation de E[Y|Z]
    data<-data.frame(y=Values[,ncol(Values)-2])
    data$z1<-Values[,ncol(Values)-1]
    data$z2<-Values[,ncol(Values)]
    reg_locpol_Y_plug<-NULL
    x__1=data$z1
    x__2=data$z2
    h_1<-locpol::pluginBw(data$z1,data$y,deg=1,kernel=kernel)
    h_2<-locpol::pluginBw(data$z2,data$y,deg=1,kernel=kernel)
    for(i in 1:length(x__1))
    {
      weight1<-NULL
      weight2<-NULL
      X_tilt_1=data$z1-x__1[i]
      X_tilt_2=data$z2-x__2[i]
      #Compute weight
      for(j in 1:length(data$z2))
      {
        weight1[j]=kernel((X_tilt_1[j]/h_1))
        weight2[j]=kernel((X_tilt_2[j]/h_2))
      }
      reg_locpol_Y_plug[i]=stats::lsfit(cbind(X_tilt_1,X_tilt_2),data$y,weight1*weight2)$coef[1]
    }
    m_Y_plug=reg_locpol_Y_plug


    #Xtilde and Ytilde
    Xtilde_plug=Values[,1:(ncol(Values)-3)]-as.numeric(m_X_plug)
    Ytilde_plug=Values[,ncol(Values)-2]-m_Y_plug

    #expectreg regression linear
    xnam <- paste("rb(Xtilde_plug,type='parametric')")
    fmla <- as.formula(paste("Ytilde_plug ~ ", paste(xnam, collapse = "+")))


    expect_linear_plug=expectreg::expectreg.ls(fmla,estimate="laws",smooth="schall",expectiles=omega)
    delta<-data.frame(expect_linear_plug$intercepts,expect_linear_plug$coefficients)
    dfnam <- paste("delta", 0:(ncol(Values)-3), sep = "")
    colnames(delta) <- dfnam



    #Nonparametric part
    Y_last=Values[,ncol(Values)-2]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-3)])-delta[,1]
    Estimates<-expectreg_loclin_bivariate(Z1=Values[,ncol(Values)-1],Z2=Values[,ncol(Values)]
                                          ,Y=as.numeric(Y_last),omega=omega,
                                          h=h_GenROT_bivariate(Z1=Values[,ncol(Values)-1],Z2=Values[,ncol(Values)]
                                                               ,Y=as.numeric(Y_last),omega=omega,kernel=kernel),grid=grid)

    l1 = list(Linear=delta,Nonlinear=Estimates)
    return(l1)
  }

  for(i in 1:ncol(X))
  {
    reg_locpol_plug<-NULL
    data<-data.frame(x=Values[,i])
    data$z1<-Values[,ncol(Values)-1]
    data$z2<-Values[,ncol(Values)]
    x__1=data$z1
    x__2=data$z2
    h_1<-locpol::pluginBw(data$z1,data$x,deg=1,kernel=kernel)
    h_2<-locpol::pluginBw(data$z2,data$x,deg=1,kernel=kernel)

    for(i in 1:length(x__1))
    {
      weight1<-NULL
      weight2<-NULL
      X_tilt_1=data$z1-x__1[i]
      X_tilt_2=data$z2-x__2[i]
      #Compute weight
      for(j in 1:length(data$z2))
      {
        weight1[j]=kernel((X_tilt_1[j]/h_1))
        weight2[j]=kernel((X_tilt_2[j]/h_2))
      }
      reg_locpol_plug[i]=stats::lsfit(cbind(X_tilt_1,X_tilt_2),data$x,weight1*weight2)$coef[1]
    }
    #vector m_X(z_i)
    m_X_plug=cbind(m_X_plug,reg_locpol_plug)
  }

  #Estimation de E[Y|Z]
  data<-data.frame(y=Values[,ncol(Values)-2])
  data$z1<-Values[,ncol(Values)-1]
  data$z2<-Values[,ncol(Values)]
  reg_locpol_Y_plug<-NULL
  x__1=data$z1
  x__2=data$z2
  h_1<-locpol::pluginBw(data$z1,data$y,deg=1,kernel=kernel)
  h_2<-locpol::pluginBw(data$z2,data$y,deg=1,kernel=kernel)
  for(i in 1:length(x__1))
  {
    weight1<-NULL
    weight2<-NULL
    X_tilt_1=data$z1-x__1[i]
    X_tilt_2=data$z2-x__2[i]
    #Compute weight
    for(j in 1:length(data$z2))
    {
      weight1[j]=kernel((X_tilt_1[j]/h_1))
      weight2[j]=kernel((X_tilt_2[j]/h_2))
    }
    reg_locpol_Y_plug[i]=stats::lsfit(cbind(X_tilt_1,X_tilt_2),data$y,weight1*weight2)$coef[1]
  }
  m_Y_plug=reg_locpol_Y_plug


  #Xtilde and Ytilde
  Xtilde_plug=Values[,1:(ncol(Values)-3)]-m_X_plug
  Ytilde_plug=Values[,ncol(Values)-2]-m_Y_plug

  #expectreg regression linear
  xnam <- paste("Xtilde_plug[,", 1:(ncol(Values)-3), sep = "")
  xnam <- paste("rb(", xnam,"],type='parametric')")
  fmla <- as.formula(paste("Ytilde_plug ~ ", paste(xnam, collapse = "+")))


  expect_linear_plug=expectreg::expectreg.ls(fmla,estimate="laws",smooth="schall",expectiles=omega)
  delta<-data.frame(expect_linear_plug$intercepts,expect_linear_plug$coefficients)
  dfnam <- paste("delta", 0:(ncol(Values)-3), sep = "")
  colnames(delta) <- dfnam



  #Nonparametric part
  Y_last=Values[,ncol(Values)-2]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-3)])-delta[,1]
  Estimates<-expectreg_loclin_bivariate(Z1=Values[,ncol(Values)-1],Z2=Values[,ncol(Values)]
                                        ,Y=as.numeric(Y_last),omega=omega,
                                        h=h_GenROT_bivariate(Z1=Values[,ncol(Values)-1],Z2=Values[,ncol(Values)]
                                                             ,Y=as.numeric(Y_last),omega=omega,kernel=kernel),grid=grid)

  l1 = list(Linear=delta,Nonlinear=Estimates)
  return(l1)
}


