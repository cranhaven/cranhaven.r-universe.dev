#' Cuestas and Ordonez(2014) nonlinear unit root test function
#'
#' This function allows you to make Cuestas and Ordonez(2014) nonlinear unit root test
#' @param x series name,
#' @param max_lags maximum lag selected lag is determined by AIC
#' @return "model" Estimated model
#' @return "Selected lag" the lag order
#' @return "Test Statistic" the value of the test statistic
#' @keywords nonlinear unit root test
#' @references
#' Cuestas, J. C., & Ordóñez, J. (2014). Smooth transitions, asymmetric adjustment and unit roots. Applied Economics Letters, 21(14), 969-972.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC nls.control embed residuals lm
#' @importFrom minpack.lm nlsLM
#' @examples
#'
#' x <- rnorm(1000)
#' Cuestas_Ordonez_2014_unit_root(x, max_lags = 6)
#'
#' y <- cumsum(rnorm(1000))
#' Cuestas_Ordonez_2014_unit_root(y, max_lags = 8)
#'
#' data(IBM)
#' Cuestas_Ordonez_2014_unit_root(IBM, max_lags = 20)
#'
#'
Cuestas_Ordonez_2014_unit_root<-function(x,max_lags){


  try({
    n=length(x)
    trend<-seq(0,length(x)-1,1)
    nonlin_model=nlsLM(x ~ a1 + a2*trend + a3*(1/(1+exp(-0.5*(trend)))) + a4*trend*(1/(1+exp(-0.5*(trend)))),start=list(a1=0,a2=0,a3=0,a4=0),control = nls.control(maxiter = 500))
  res<-residuals(nonlin_model)
  },silent = T)


KSS_unit_root(x=res,case=1,max_lags=max_lags)


}


KSS_unit_root<-function(x,case,max_lags){
  if(case==1){
    x=x
  }
  if(case==2){
    x=x-mean(x)
  }
  if(case==3){
    trend<-seq(0,length(x)-1,1)
    mod=lm(x~trend)
    x=mod$residuals
  }

  say=99999999999999999999999999999
  for(i in 1:max_lags){
    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    kup=x^3
    z.lag.1=kup[(i+1):n]
    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~z.lag.1+0+z.diff.lag )
    son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
      if (AIC(model)<say){
      uygun_lag=i
      say=AIC(model)
    }
    z.diff=embed(z, uygun_lag+1)[,1]
    kup=x^3
    z.lag.1=kup[(uygun_lag+1):n]
    k=uygun_lag+1
    z.diff.lag = embed(z, uygun_lag+1)[, 2:k]
    temps=(uygun_lag+1):n
    model<-lm(z.diff~z.lag.1+0+z.diff.lag )
    son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
    my_list <- list("model"=summary(model),"Selected lag"=uygun_lag, "Test Statistic"=son)
    return(my_list)
  }

}
