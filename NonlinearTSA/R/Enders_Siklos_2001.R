#' Enders and Siklos(2001) Nonlinear Cointegration Test Function
#'
#' This function allows you to make Enders and Siklos(2001) nonlinear cointegration test
#' @param x series name,
#' @param y series name
#' @param case if no lag 1, if one lag 2, if four lag 3, default case=2
#' @param max_lags maximum lag (Apropriate lag is selected by Akaike Information Criteria)
#' @return "Model" Estimated model
#' @return "Selected Lag" the lag order
#' @return "p1=p2=0 Statistic" the value of the test statistic
#' @return "p1=p2 Statistic" the value of the test statistic
#' @return "p value" the probability of test statistic
#' @keywords nonlinear cointegration test
#' @references
#' Enders, W., & Siklos, P. L. (2001). Cointegration and threshold adjustment. Journal of Business & Economic Statistics, 19(2), 166-176.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC residuals embed
#' @importFrom tsDyn setar
#' @importFrom car linearHypothesis
#' @examples
#' x <- cumsum(rnorm(1000))
#' y <- cumsum(rnorm(1000))
#' Enders_Siklos_2001(x, y, max_lags = 6)
#'
#' data(MarketPrices)
#' Enders_Siklos_2001(MarketPrices[,1],MarketPrices[,2], max_lags = 12)
#'
Enders_Siklos_2001 <- function(y,x,case=2,max_lags){
y = as.vector(y)
x = as.vector(x)

  res = residuals(lm(y~x))
  z=diff(res)
  if(case==1){
  gfz=embed(res,1)[,1]
  }
  if(case==2){
    gfz=embed(res,2)[,2]
  }
  if(case==3){
    gfz=embed(res,4)[,4]
  }

  say=99999999999999999999999999999
  for(i in 1:max_lags){

    model = setar(gfz,m=max_lags,model="MTAR",th=0,type="ADF",common="both", include = "none")
    if (AIC(model)<say){
      uygun_lag=i
      say=AIC(model)
    }
    model = setar(gfz,m=uygun_lag,model="MTAR",th=0,type="ADF",common="both", include = "none")
  }

  p1 = linearHypothesis(model, c("phiL.1=0", "phiH.1=0"), test="F")
  p2 = linearHypothesis(model, c("phiL.1=phiH.1"), test="F")
  tval=p1[,"Chisq"][2]
  tval2=p2[,"Chisq"][2]

  my_list <- list("Model"=summary(model), "Selected Lag"=uygun_lag, "p1=p2=0 Statistic"=tval/2, "p1=p2 Statistic"=tval2, "p value"=p2[,"Pr(>Chisq)"][2])
  return(my_list)
}


