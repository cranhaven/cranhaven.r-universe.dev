#' Sollis(2009) nonlinear unit root test function
#'
#'  This function allows you to make Sollis(2009) nonlinear unit root test
#' @param x series name,
#' @param case if raw data 1 if demeaned data 2 if detrended data 3,
#' @param lags maximum lag
#' @param lsm lag selection methods if 1 AIC, if 2 BIC, if 3 t-stat significance
#' @return "Model" Estimated model
#' @return "Selected lag" the lag order
#' @return "Test Statistic" the value of the test statistic
#' @references
#' Sollis, R. (2009). A simple unit root test against asymmetric STAR nonlinearity with an application to real exchange rates in Nordic countries. Economic modelling, 26(1), 118-125.
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#'
#' @keywords nonlinear unit root test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom stats AIC BIC residuals embed lm
#' @examples
#' x <- rnorm(1000)
#' Sollis2009_Unit_Root(x, case = 1, lags = 6, lsm = 3)
#'
#'
#' y <- cumsum(rnorm(1000))
#' Sollis2009_Unit_Root(y, 3, 8, 1)
#'
#'
#' data(IBM)
#' Sollis2009_Unit_Root(IBM,case = 2,lags = 12,lsm = 2)
#'
#'
Sollis2009_Unit_Root<-function(x,case,lags,lsm){
  if(lags == 0){
    stop("\n lags must be grater than 0")
  }
  x = as.vector(x)
  if(case == 1){
    x = x
  }
  if(case == 2){
    x = x-mean(x)
  }
  if(case == 3){
    trend <- seq(0,length(x)-1,1)
    mod = lm(x~trend)
    x = residuals(mod)
  }

  AICs = NULL
  BICs = NULL
  tstats = NULL

  for(i in 1:lags){
    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    kup=x^3
    z.lag.1=kup[(i+1):n]
    karekare=x^4
    z.lag.2=karekare[(i+1):n]

    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~z.lag.1+z.lag.2+0+z.diff.lag )

    AICs[i+1] = AIC(model)
    BICs[i+1] = BIC(model)
    tstats[i+1] = summary(model)$coefficients[(i+2),4]
  }
  z=diff(x)
  n=length(z)
  z.diffzero=embed(z, 2)[,1]
  kupzero=x^3
  z.lag.zero.1=kupzero[2:n]
  karekarezero=x^4
  z.lag.zero.2=karekarezero[2:n]
  model0<-lm(z.diffzero~z.lag.zero.1+z.lag.zero.2+0)
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
  karekare=x^4
  z.lag.2=karekare[(uygun_lag+1):n]
  k=uygun_lag+1
  if(uygun_lag == 0){
    model = lm(z.diff~z.lag.1+z.lag.2+0)
    son <- lm(z.diff~z.lag.1+z.lag.2+0)
    ay <- linearHypothesis(son, c("z.lag.1=0","z.lag.2=0"), test="F")
  } else {
    z.diff.lag = embed(z, uygun_lag+1)[, 2:k]
    model = lm(z.diff~z.lag.1+z.lag.2+0+z.diff.lag)
    son <- lm(z.diff~z.lag.1+z.lag.2+0+z.diff.lag)
    ay <- linearHypothesis(son, c("z.lag.1=0","z.lag.2=0"), test="F")
  }
  my_list <- list("Model" = summary(model), "Selected lag"=uygun_lag, "Test Statistic"=ay$F[2])
  return(my_list)
}


