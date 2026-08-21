#' Kilic(2011) nonlinear unit root test function
#'
#' This function allows you to make Kilic(2011) nonlinear unit root test
#' @param x series name,
#' @param case if raw data 1 if demeaned data 2 if detrended data 3,
#' @param max_lags maximum lag apropriate lag length is selected by Akaike Information Criteria
#' @return "Model" Estimated model
#' @return "Selected Lag" the lag order
#' @return "Test statistic" the value of the test statistic
#' @keywords nonlinear unit root test
#' @references
#' Kılıç, R. (2011). Testing for a unit root in a stationary ESTAR process. Econometric Reviews, 30(3), 274-302.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC sd lm embed residuals
#' @examples
#'
#'\donttest{
#'x <- rnorm(100)
#'Kilic_2011_unit_root(x,1,3)
#'
#'data(IBM)
#'Kilic_2011_unit_root(IBM, case = 3, max_lags = 12)
#'
#'}
#'

Kilic_2011_unit_root<-function(x,case,max_lags){
  x = as.vector(x)
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

  z=diff(x)
  as=sd(z)
  alt=1/(100*as)
  ust=100/as

  say=-99999999999999999999999999999
  for(ye in seq(alt, ust, by=0.01)){


  ii=1
  z=diff(x)
  n=length(z)
  z.diff=embed(z, ii+1)[,1]
  z.lag.1=x[(ii+1):n]
  k=ii+1
  z.diff.lag = embed(z, ii+1)[, 2:k]
  model<-lm(z.diff~z.lag.1*(1-exp(-ye*z.diff^2))+0+z.diff.lag )
  son<-summary(model)$coefficients[1,3]

  if (son>say){
    uygun_ye=ye
    say=son
  }

  uygun_model<-lm(z.diff~z.lag.1*(1-exp(-uygun_ye*z.diff^2))+0+z.diff.lag )
  uygun_son<-summary(model)$coefficients[1,3]

  }


  saysay=99999999999999999999999999999

  for(i in 1:max_lags){

    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    kup=x
    z.lag.1=kup[(i+1):n]
    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    en_uygun_model<-lm(z.diff~z.lag.1*(1-exp(-uygun_ye*z.diff^2))+0+z.diff.lag )
    en_uygun_son<-summary(model)$coefficients[1,3]

    if (AIC(en_uygun_model)<saysay){
      uygun_lag=i
      saysay=AIC(en_uygun_model)
    }

    z.diff=embed(z, uygun_lag+1)[,1]
    kup=x^3
    z.lag.1=kup[(uygun_lag+1):n]
    k=uygun_lag+1
    z.diff.lag = embed(z, uygun_lag+1)[, 2:k]
    temps=(uygun_lag+1):n
    en_en_uygun_model<-lm(z.diff~z.lag.1*(1-exp(-uygun_ye*z.diff^2))+0+z.diff.lag )
    en_en_uygun_son<-summary(en_uygun_model)$coefficients[1,3]

  }

  my_list <- list("Model"=summary(en_en_uygun_model),"Selected Lag"= uygun_lag, "Test statistic"=en_en_uygun_son)
  return(my_list)

}

