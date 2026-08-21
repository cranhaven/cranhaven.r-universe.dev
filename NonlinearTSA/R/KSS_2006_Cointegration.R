#' Kapetanios, Shin and Snell(2006) nonlinear cointegration test function
#'
#' This function allows you to make Kapetanios, Shin and Snell(2006) nonlinear cointegration
#' test using residual based approach
#' @param y series name,
#' @param x series name
#' @param case if raw data 1 if demeaned data 2 if detrended data 3,
#' @param lags lag length
#' @param lsm lag selection methods if 1 AIC, if 2 BIC, if 3 t-stat significance
#' @return "Model" Estimated model
#' @return "Selected lag" the lag order
#' @return "Test Statistic" the value of the test statistic
#' @keywords STAR vector error correction model
#' @references
#' Kapetanios, G., Shin, Y., & Snell, A. (2006). Testing for cointegration in nonlinear smooth transition error correction models. Econometric Theory, 22(2), 279-303.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC BIC lm residuals embed
#' @importFrom car linearHypothesis
#' @examples
#' x <- cumsum(rnorm(1000))
#' y <- cumsum(rnorm(1000))
#' KSS_2006_Cointegration(x, y, case = 1, lags = 6, lsm = 3)
#'
#'
#' KSS_2006_Cointegration(MarketPrices[,1],MarketPrices[,2], case = 1, lags = 2, lsm = 1)
#'
KSS_2006_Cointegration<-function(y, x, case, lags, lsm){
  y = as.vector(y)
  x = as.vector(x)



  if(case==1){
    res = residuals(lm(y~x-1))
    u=res
  }
  if(case==2){
    res = residuals(lm(I(y-mean(y))~I(x-mean(x))-1))
    u=res
  }
  if(case==3){
    trend<-seq(0,length(x)-1,1)
    mod1=lm(y~trend)
    y1=residuals(mod1)

    mod2=lm(x~trend)
    x1=residuals(mod2)
    res = residuals(lm(y1~x1-1))
    u=res
  }

  AICs = NULL
  BICs = NULL
  tstats = NULL
  x = as.matrix(u)
  for(i in 1:lags){
    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    kup=x^3
    z.lag.1=kup[(i+1):n]
    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~z.lag.1+0+z.diff.lag )
    son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
    AICs[i+1] = AIC(model)
    BICs[i+1] = BIC(model)
    tstats[i+1] = summary(model)$coefficients[(i+1),4]
  }

  z=diff(x)
  n=length(z)
  z.diffzero=embed(z, 2)[,1]
  kupzero=x^3
  z.lag.zero.1=kupzero[2:n]
  model0<-lm(z.diffzero~z.lag.zero.1+0)
  sonzero<-summary(lm(z.diffzero~z.lag.zero.1+0))$coefficients[1,3]
  AICs[1] = AIC(model0)
  BICs[1] = BIC(model0)
  tstats[1] = 0.0000

  if(lsm == 1){
    uygun_lag=which.min(AICs)-1
  }
  else if(lsm == 2){
    uygun_lag=which.min(BICs)-1
  }
  else {

    for (ti in (lags+1):1){
      if (tstats[ti] <= 0.10){
        uygun_lag = ti-1
        break
      }
    }
  }
  z.diff=embed(z, uygun_lag+1)[,1]
  kup=x^3
  z.lag.1=kup[(uygun_lag+1):n]
  k=uygun_lag+1
  if(uygun_lag == 0){
    model = lm(z.diff~z.lag.1+0)
    son<-summary(lm(z.diff~z.lag.1+0 ))$coefficients[1,3]
  } else {
    z.diff.lag = embed(z, uygun_lag+1)[, 2:k]
    model = lm(z.diff~z.lag.1+0+z.diff.lag)
    son<-summary(lm(z.diff~z.lag.1+0+z.diff.lag ))$coefficients[1,3]
  }
  my_list <- list("Model"=summary(model),"Selected lag"=uygun_lag, "Test Statistic"=son)
  return(my_list)

}


