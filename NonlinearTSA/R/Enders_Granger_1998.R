#' Enders and Granger_1998 nonlinear unit root test function
#'
#' This function allows you to make Enders and Granger(1998) nonlinear unit root test for MTAR model
#' @param x series name,
#' @param case if raw data 1 if demeaned data 2 if detrended data 3,
#' @param max_lags maximum lag
#' @param lsm lag selection methods if 1 AIC, if 2 BIC
#' @return "Model" Estimated model
#' @return "Selected lag" the lag order
#' @return "p1=p2=0 Statistic" the value of the test statistic
#' @return "p1=p2 statistic" the value of the test statistic
#' @return "prob." the probability of test statistic
#' @references
#' Enders, W., & Granger, C. W. J. (1998). Unit-root tests and asymmetric adjustment with an example using the term structure of interest rates. Journal of Business & Economic Statistics, 16(3), 304-311.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @keywords nonlinear unit root test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom stats AIC BIC
#' @importFrom tsDyn setar
#' @examples
#'
#' x <- rnorm(1000)
#' Enders_Granger_1998(x, case = 1, max_lags = 6, lsm = 1)
#'
#' y <- cumsum(rnorm(1000))
#' Enders_Granger_1998(y, 2, 8, 2)
#'
#' data(IBM)
#' Enders_Granger_1998(IBM,case = 2,max_lags = 12,lsm = 2 )
#'
#'

Enders_Granger_1998 <- function(x,case,max_lags,lsm){

  say=99999999999999999999999999999
  if (lsm == 1){
    a <- AIC
  } else {
    a <- BIC
  }

  if (case==1){
      for(i in 1:max_lags){

      model = setar(x,m=max_lags,model="MTAR",th=0,type="ADF",common="both", include = "none")
      if (a(model)<say){
        uygun_lag=i
        say=a(model)
      }
      model = setar(x,m=uygun_lag,model="MTAR",th=0,type="ADF",common="both", include = "none")
    }
  }
  if (case==2){
    for(i in 1:max_lags){

      model = setar(x,m=max_lags,model="MTAR",th=0,type="ADF",common="both", include = "const")
      if (a(model)<say){
        uygun_lag=i
        say=a(model)
      }
      model = setar(x,m=uygun_lag,model="MTAR",th=0,type="ADF",common="both", include = "const")
    }
  }
  if (case==3){
    for(i in 1:max_lags){

      model = setar(x,m=max_lags,model="MTAR",th=0,type="ADF",common="both", include = "both")
      if (a(model)<say){
        uygun_lag=i
        say=a(model)
      }
      model = setar(x,m=uygun_lag,model="MTAR",th=0,type="ADF",common="both", include = "both")
    }
  }

  p1 = linearHypothesis(model, c("phiL.1=0", "phiH.1=0"), test="F")
  p2 = linearHypothesis(model, c("phiL.1=phiH.1"), test="F")
  tval=p1[,"Chisq"][2]
  tval2=p2[,"Chisq"][2]
  tval2ol=p2[,"Pr(>Chisq)"][2]

  my_list <- list("Model"=summary(model), "Selected lag"=uygun_lag,"p1=p2=0 Statistic"=tval/2, "p1=p2 statistic"=tval2, "prob."=tval2ol)
  return(my_list)
}

