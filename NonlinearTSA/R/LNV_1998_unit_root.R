#' Leybourne Newbold and Vougas (1998) nonlinear unit root test function
#'
#' This function allows you to make Leybourne, Newbold and Vougas (1998) nonlinear unit root test
#' @param x series name,
#' @param model if model with intercept 1, if model with trend 2 if model with trend*function 3,
#' @param max_lags maximum lag
#' @param lsm lag selection methods if 1 AIC, if 2 BIC
#' @return "Model" Estimated model
#' @return "Selected Lag" the lag order
#' @return "Test Statistic" the value of the test statistic
#' @references
#' Leybourne, S., Newbold, P., & Vougas, D. (1998). Unit roots and smooth transitions. Journal of time series analysis, 19(1), 83-97.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @keywords nonlinear unit root test
#' @export
#' @importFrom stats AIC BIC residuals lm embed
#' @importFrom minpack.lm nlsLM
#' @examples
#'\donttest{
#'x <- rnorm(1000)
#'LNV_1998_unit_root(x, model = 1, max_lags = 6, lsm = 2)
#'
#'
#'y <- cumsum(rnorm(1000))
#'LNV_1998_unit_root(y, 3,  3, lsm = 1)
#'
#'
#'data(IBM)
#'LNV_1998_unit_root(x = IBM, model=2,max_lags = 10, lsm = 1)
#'
#'}
#'

LNV_1998_unit_root<-function(x,model,max_lags,lsm){

  res<-LNV_1998_unit_root_fist_step(x,model,max_lags)$res
  ADF_unit_root(res,max_lags,lsm)
}

LNV_1998_unit_root_fist_step<-function(x,model,max_lags){

  if (model==1){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))),start=list(a1=0,a2=0,a3=1,a4=0.5),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  if (model==2){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))) + a5*trend,start=list(a1=0,a2=0,a3=1,a4=0.5,a5=0),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  if (model==3){
    try({
      n=length(x)
      trend<-seq(0,length(x)-1,1)
      nonlin_model=nlsLM(x ~ a1 + a2*(1/(1+exp(-a3*(trend-(a4*n))))) + a5*trend + a6*trend*(1/(1+exp(-a3*(trend-(a4*n))))), start=list(a1=0,a2=0,a3=1,a4=0.5,a5=0,a6=0),control = nls.control(maxiter = 500))
      res=residuals(nonlin_model)
    },silent = T)
  }
  my_list = list("res"=res)
  return(my_list)
}



ADF_unit_root <- function(x,max_lags,lsm){

  AICs = NULL
  BICs = NULL

  for(i in 1:max_lags){


    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    z.lag.1=x[(i+1):n]
    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~z.lag.1+0+z.diff.lag )
    son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
    AICs[i+1] = AIC(model)
    BICs[i+1] = BIC(model)

    z.diffzero=embed(z, 2)[,1]
    z.lag.zero.1=x[2:n]
    model0<-lm(z.diffzero~z.lag.zero.1+0)

    AICs[1] = AIC(model0)
    BICs[1] = BIC(model0)
    if(lsm == 1){
      uygun_lag=which.min(AICs)-1
    } else {
      uygun_lag=which.min(BICs)-1
    }
    z.diff=embed(z, uygun_lag+1)[,1]
    z.lag.1=x[(uygun_lag+1):n]

    if(uygun_lag == 0){
      model = lm(z.diff~z.lag.1+0)
      son<-summary(lm(z.diff~z.lag.1+0 ))$coefficients[1,3]
    } else {
      z.diff.lag = embed(z, uygun_lag+1)[, 2:k]
      model = lm(z.diff~z.lag.1+0+z.diff.lag)
      son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
    }

  }
  my_list <- list("Model" = summary(model),"Selected Lag"=uygun_lag, "Test Statistic"=son)
  return(my_list)
}



