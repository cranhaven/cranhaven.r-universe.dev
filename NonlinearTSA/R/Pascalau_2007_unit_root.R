#' Pascalau(2007) nonlinear unit root test function
#'
#' This function allows you to make Pascalau(2007) nonlinear unit root test
#' @param x series name,
#' @param case if raw data 1, if demeaned data 2, if detrended data 3
#' @param max_lags maximum lag
#' @param lsm lag selection methods if 1 AIC, if 2 BIC
#' @return "Model" Estimated model
#' @return "Selected lag" the lag order
#' @return "Test statistic" the value of the test statistic
#' @references
#' Pascalau, R. (2007). Testing for a unit root in the asymmetric nonlinear smooth transition framework. Department of Economics, Finance and Legal Studies University of Alabama Unpublished manuscript.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#'
#' @keywords nonlinear unit root test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom stats AIC BIC residuals lm
#' @examples
#' x <- rnorm(1000)
#' Pascalau_2007_unit_root(x, case = 1, max_lags = 6, lsm = 2)
#'
#'
#' y <- cumsum(rnorm(1000))
#' Pascalau_2007_unit_root(y, 2, 4, 1)
#'
#'
#' data(IBM)
#' Pascalau_2007_unit_root(x = IBM, case = 3, max_lags = 3, lsm = 1)
#'
#'

Pascalau_2007_unit_root<-function(x,case,max_lags,lsm){
  if(max_lags == 0){
    stop("\n lags must be grater than 0")
  }
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
    x = residuals(mod)
  }

  say=99999999999999999999999999999
  for(i in 1:max_lags){
    z=diff(x)
    n=length(z)
    z.diff=embed(z, i+1)[,1]
    karekare=x^4
    z.lag.4=karekare[(i+1):n]
    kup=x^3
    z.lag.3=kup[(i+1):n]
    kare=x^2
    z.lag.2=kare[(i+1):n]

    beta_4=z.lag.4
    beta_3=z.lag.3
    beta_2=z.lag.2

    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~beta_4+beta_3+beta_2+0+z.diff.lag )
    ay<-linearHypothesis(model, c("beta_4=0","beta_3=0","beta_2=0"), test="F")
    if(lsm==1){a = AIC} else {a = BIC}

    if (a(model)<say){
      uygun_lag=i
      say=a(model)
    }
    z.diff=embed(z, uygun_lag+1)[,1]
    karekare=x^4
    z.lag.4=karekare[(uygun_lag+1):n]
    kup=x^3
    z.lag.3=kup[(uygun_lag+1):n]
    kare=x^2
    z.lag.2=kare[(uygun_lag+1):n]
    beta_4=z.lag.4
    beta_3=z.lag.3
    beta_2=z.lag.2
    k=i+1
    z.diff.lag = embed(z, i+1)[, 2:k]
    model<-lm(z.diff~beta_4+beta_3+beta_2+0+z.diff.lag)
    ay<-linearHypothesis(model, c("beta_4=0","beta_3=0","beta_2=0"), test="F")
    my_list <- list("Model" = summary(model), "Selected lag"=uygun_lag, "Test statistic"=ay$F[2])
    return(my_list)
  }

}
