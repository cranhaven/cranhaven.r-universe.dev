#' Park and Shintani(2012) nonlinear unit root test function
#'
#' This function allows you to make Park and Shintani(2012) nonlinear unit root test
#' @param x series name,
#' @param max_lags maximum lag (Apropriate lag is selected by Akaike Information Criteria)
#' @return "Model" Estimated model
#' @return "Selected Lag" the lag order
#' @return "Test statistic" the value of the test statistic
#' @keywords nonlinear unit root test
#' @references
#' Park, J. Y., & Shintani, M. (2016). Testing for a unit root against transitional autoregressive models. International Economic Review, 57(2), 635-664.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC embed lm
#' @examples
#'
#'\donttest{
#'x <- rnorm(50)
#'Park_Shintani_2016_unit_root(x, max_lags = 1)
#'
#'data(IBM)
#'Park_Shintani_2016_unit_root(IBM, max_lags = 12)
#'
#'}
#'
#'

Park_Shintani_2016_unit_root<-function(x,max_lags){
  x = as.vector(x)

  z=x
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
    kup=x^3
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

