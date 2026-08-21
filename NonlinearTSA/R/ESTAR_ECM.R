#' STAR Vector Error Correction Model
#'
#' This function allows you to estimate ESTAR Vector Error Correction Model
#' @param y series name,
#' @param x series name
#' @param lags lag length
#' @return "Model" Estimated model
#' @return "AIC" Akaike information criteria
#' @return "BIC" Schwarz information criteria
#' @keywords STAR vector error correction model
#' @details Exponential smooth transition error correction model as follows:
#' @references
#' Kapetanios, G., Shin, Y., & Snell, A. (2006). Testing for cointegration in nonlinear smooth transition error correction models. Econometric Theory, 22(2), 279-303.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC BIC embed residuals lm
#' @importFrom car linearHypothesis
#' @examples
#' x <- cumsum(rnorm(1000))
#' y <- cumsum(rnorm(1000))
#' ESTAR_ECM(x, y, lags = 6)
#'
#' data(MarketPrices)
#' ESTAR_ECM(MarketPrices[,1],MarketPrices[,2],lags = 2)
#'
ESTAR_ECM<-function(y,x,lags){
  y = as.vector(y)
  x = as.vector(x)

  res = residuals(lm(y~x))
  z=res
  gfz=embed(z,2)[,2]

  k=lags+1
  ygecikme = embed(diff(y),lags+1)[,2:k]
  xgecikme = embed(diff(x),lags+1)#[,2:k]

  g1 = matrix(NA, nrow = lags+1, ncol = ncol(cbind(xgecikme)))
  g2 = matrix(NA, nrow = lags+1, ncol = ncol(cbind(ygecikme)))

  xf = rbind(g1,cbind(xgecikme))
  yf = rbind(g2,cbind(ygecikme))
  gfzesas = rbind(cbind(rep(NA, times=1)),cbind(gfz))
  ybagli = rbind(cbind(rep(NA, times=1)),cbind(diff(y)))

  DX = xf
  DY1 = yf

  u = gfzesas
  ESTAR_ECM = lm(ybagli~I(u^3)+DX+DY1-1)
  my_list<-list("Model"=ESTAR_ECM, "AIC" = AIC(ESTAR_ECM),"BIC"=BIC(ESTAR_ECM))
  return(my_list)
}

