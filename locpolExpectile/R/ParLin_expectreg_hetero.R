#' @title Partially linear expectile regression with different possible heteroscedastic error and
#' univariate variable in the nonparametric function
#'
#' @description Formula interface for the partially linear expectile regression
#' using local linear expectile estimation for different heteroscedastic error structure and
#' a univariate variable in the nonparametric function \eqn{g(.)}. The model is of the form
#' \eqn{Y=\delta^T X + g(Z) + \sigma(X) \epsilon}, \eqn{Y=\delta^T X + g(Z) + \sigma(Z) \epsilon}
#' or \eqn{Y=\delta^T X + g(Z) + \sigma(Z,X) \epsilon}.
#' See Table 1 in Adam and Gijbels (2021b) for more details.
#'
#' @param X The covariates data values for the linear part
#' (of size \eqn{n \times k} with \code{k=1} or \code{k=2}).
#' @param Y The response data values.
#' @param Z The covariate data values for the nonparametric part.
#' @param omega Numeric vector of level between 0 and 1 where 0.5 corresponds
#' to the mean.
#' @param kernel The kernel used to perform the estimation. In default setting,
#' \code{kernel=gaussK}. See details in \code{\link[locpol]{Kernels}}.
#' @param heteroscedastic Heteroscedastic error depending on \code{X}, or on\code{Z}
#' or on \code{Z} and \code{X}
#'
#' @return \code{\link{ParLin_expectreg_hetero}} partially linear expectile estimators
#' for different heteroscedastic error structures and a univariare variable in the nonparametric part,
#' proposed and studied by Adam and Gijbels (2021b). \code{\link{ParLin_expectreg_hetero}}
#' returns a list whose components are:
#' \itemize{
#' \item If the heteroscedastic error depends on \eqn{Z}:
#' \itemize{
#'  \item \code{Linear} The delta estimators for the linear part
#'  \item \code{Nonlinear} The estimation of the nonparametric part
#'  according to the observed values \eqn{Z_i}.
#' }
#' \item If the heteroscedastic error depends on \eqn{X}:
#' \itemize{
#'  \item \code{Linear} The delta estimators for the linear part
#'  \item \code{Nonlinear_g} The estimation of the nonparametric part
#'  according to the observed values \eqn{Z_i}.
#'  \item \code{Nonlinear_g_omega} The estimation of
#'  the nonparametric part according to the observed values \eqn{X_i} (if \eqn{X} is univariate)
#'  or to the couple of observed values \eqn{(X_{1i},X_{2j})}.
#' }
#' \item If the heteroscedastic error depends on \eqn{Z} and \eqn{X}:
#' \itemize{
#'  \item \code{Linear} The delta estimators for the linear part
#'  \item \code{Nonlinear_g} The estimation of the nonparametric part
#'  according to the couple of the observed values \eqn{(Z_i,X_j)} (if \eqn{X} is univariate)
#'  or to the observed values \eqn{(Z_i,X_{1i},X_{2i})}.
#' }
#'
#' }
#'
#' @import expectreg
#' @import locpol
#' @import stats
#' @rdname ParLin_expectreg_hetero
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
#' \donttest{
#' library(locpol)
#' set.seed(123)
#' Z<-runif(100,-3,3)
#' eta_1<-rnorm(100,0,1)
#' X1<-(0.9*Z)+(1.5*eta_1)
#' set.seed(1234)
#' eta_2<-rnorm(100,0,2)
#' X2<-(0.9*Z)+(1.5*eta_2)
#' X<-rbind(X1,X2)
#'
#' set.seed(12345)
#' epsilon<-rnorm(100,0,1)
#' delta<-rbind(0.8,-0.8)
#'
#' Y<-as.numeric((t(delta)%*%X)+(10*sin(0.9*Z))+(0.6*X1^2)*epsilon)
#'
#' ParLin_expectreg_hetero(X=t(X),Y=Y,Z=Z,omega=0.3,kernel=gaussK,heteroscedastic="X")
#' }
#' @name ParLin_expectreg_hetero
#' @export
#'

ParLin_expectreg_hetero<-function(X,Y,Z,omega=0.3,kernel=gaussK,heteroscedastic=c("X", "Z", "Z and X") )
  #Estimation
{

  Values=cbind(X,Y,Z)
  Values=Values[order(Values[,ncol(Values)-1]),]
  m_X_plug<-NULL
  #E[X|Z=Z_i]
  if(NCOL(X)==1)
  {
    for(i in 1:NCOL(X))
    {
      data<-data.frame(x=Values[,i])
      data$z<-Values[,ncol(Values)]
      #vector m_X(z_i)
      m_X_plug=cbind(m_X_plug,locpol::locLinSmootherC(data$z, data$x, xeval=data$z, bw=locpol::pluginBw(data$z, data$x, deg=1,kernel=kernel),kernel=kernel)[,2])
    }

    #Estimation de E[Y|Z]
    data<-data.frame(y=Values[,ncol(Values)-1])
    data$z<-Values[,ncol(Values)]
    #vector m_Y(z_i)
    m_Y_plug=locpol::locLinSmootherC(data$z, data$y, xeval=data$z, bw=locpol::pluginBw(data$z, data$y, deg=1,kernel=kernel),kernel=kernel)[,2]

    #Xtilde and Ytilde
    Xtilde_plug=Values[,1:(ncol(Values)-2)]-as.numeric(m_X_plug)
    Ytilde_plug=Values[,ncol(Values)-1]-m_Y_plug

    #expectreg regression linear
    fmla <- as.formula(paste("Ytilde_plug ~Xtilde_plug "))


    expect_linear_plug=expectreg::expectreg.ls(fmla,estimate="laws",smooth="schall",expectiles=omega)
    delta<-data.frame(expect_linear_plug$intercepts,expect_linear_plug$coefficients)
    dfnam <- paste("delta", 0:(ncol(Values)-2), sep = "")
    colnames(delta) <- dfnam

    #Nonparametric part

    if(heteroscedastic=="Z")
    {
      grid=Values[,ncol(Values)]
      Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
      Estimates<-expectreg_locpol(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,omega=omega,h=h_GenROT(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,kernel=kernel,omega),kernel=kernel,starting_value = "mean",grid=grid)[,1]

      l1 = list(Linear=delta,Nonlinear=Estimates)
    }

    if(heteroscedastic=="X")
    {
      grid=Values[,ncol(Values)]
      Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
      Estimates_g<-expectreg_locpol(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,omega=omega,h=h_GenROT(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,kernel=kernel,omega),kernel=kernel,starting_value = "mean",grid=grid)[,1]
      Y_g_omega=Y_last-Estimates_g
      Estimates_g_omega<-expectreg_locpol(X=Values[,1],Y=as.numeric(Y_g_omega),omega=omega
                                                    ,kernel=kernel,h=h_GenROT(X=Values[,1],Y=as.numeric(Y_g_omega),omega=omega,kernel=kernel),starting_value="mean",grid=cbind(Values[,1:(ncol(Values)-2)]))[,1]
      l1 = list(Linear=delta,Nonlinear_g=Estimates_g,Nonlinear_g_omega=Estimates_g_omega)
    }
    if(heteroscedastic=="Z and X")
    {
      grid=Values[,ncol(Values)]
      Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
      Estimates<-expectreg_loclin_bivariate(Z1=Values[,ncol(Values)],Z2=Values[,1],Y=as.numeric(Y_last),omega=omega
                                 ,kernel=kernel,h=h_GenROT_bivariate(Z1=Values[,ncol(Values)],Z2=Values[,1],Y=as.numeric(Y_last),omega=omega,kernel=kernel),grid=cbind(Values[,ncol(Values)],Values[,1]))

      l1 = list(Linear=delta,Nonlinear=Estimates)
    }
  }
  for(i in 1:ncol(X))
  {
    data<-data.frame(x=Values[,i])
    data$z<-Values[,ncol(Values)]
    #vector m_X(z_i)
    m_X_plug=cbind(m_X_plug,locpol::locLinSmootherC(data$z, data$x, xeval=data$z, bw=locpol::pluginBw(data$z, data$x, deg=1,kernel=kernel),kernel=kernel)[,2])
  }

  #Estimation de E[Y|Z]
  data<-data.frame(y=Values[,ncol(Values)-1])
  data$z<-Values[,ncol(Values)]
  #vector m_Y(z_i)
  m_Y_plug=locpol::locLinSmootherC(data$z, data$y, xeval=data$z, bw=pluginBw(data$z, data$y, deg=1,kernel=kernel),kernel=kernel)[,2]

  #Xtilde and Ytilde
  Xtilde_plug=Values[,1:(ncol(Values)-2)]-m_X_plug
  Ytilde_plug=Values[,ncol(Values)-1]-m_Y_plug

  #expectreg regression linear
  xnam <- paste("Xtilde_plug[,", 1:(ncol(Values)-2), sep = "")
  xnam <- paste("rb(", xnam,"],type='parametric')")
  fmla <- as.formula(paste("Ytilde_plug ~ ", paste(xnam, collapse = "+")))


  expect_linear_plug=expectreg::expectreg.ls(fmla,estimate="laws",smooth="schall",expectiles=omega)
  delta<-data.frame(expect_linear_plug$intercepts,expect_linear_plug$coefficients)
  dfnam <- paste("delta", 0:(ncol(Values)-2), sep = "")
  colnames(delta) <- dfnam

  #Nonparametric part

  if(heteroscedastic=="Z")
  {
    grid=Values[,ncol(Values)]
    Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
    Estimates<-expectreg_locpol(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,omega=omega,h=h_GenROT(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,kernel=kernel,omega),kernel=kernel,starting_value = "mean",grid=grid)[,1]

    l1 = list(Linear=delta,Nonlinear=Estimates)
  }

  if(heteroscedastic=="X")
  {
    grid=Values[,ncol(Values)]
    Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
    Estimates_g<-expectreg_locpol(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,omega=omega,h=h_GenROT(X=Values[,ncol(Values)],Y=as.numeric(Y_last),j=0,p=1,kernel=kernel,omega),kernel=kernel,starting_value = "mean",grid=grid)[,1]
    Y_g_omega=Y_last-Estimates_g
    Estimates_g_omega<-expectreg_loclin_bivariate(Z1=Values[,1],Z2=Values[,2],Y=as.numeric(Y_g_omega),omega=omega
                                                        ,kernel=kernel,h=h_GenROT_bivariate(Z1=Values[,1],Z2=Values[,2],Y=as.numeric(Y_g_omega),omega=omega,kernel=kernel),grid=cbind(Values[,1:(ncol(Values)-2)]))
    l1 = list(Linear=delta,Nonlinear_g=Estimates_g,Nonlinear_g_omega=Estimates_g_omega)
  }

  if(heteroscedastic=="Z and X")
  {
    grid=Values[,ncol(Values)]
    Y_last=Values[,ncol(Values)-1]-(as.matrix(delta[,2:ncol(delta)]))%*%t(Values[,1:(ncol(Values)-2)])-delta[,1]
    Estimates<-expectreg_loclin_trivariate(Z=Values[,ncol(Values)],X1=Values[,ncol(Values)-3],X2=Values[,ncol(Values)-2],Y=as.numeric(Y_last),omega=omega,h=1,kernel=kernel)
    l1 = list(Linear=delta,Nonlinear=Estimates)
  }


  return(l1)
}
