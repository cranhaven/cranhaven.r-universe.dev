#' MTAR Vector Error Correction Model
#'
#' This function allows you to estimate MTAR Vector Error Correction Model with threshold=0
#' @param y series name,
#' @param x series name
#' @param lags lag length
#' @return "Model" Estimated model
#' @return "AIC" Akaike information criteria
#' @return "BIC" Schwarz information criteria
#' @keywords MTAR vector error correction model
#' @references
#' Enders, W., & Siklos, P. L. (2001). Cointegration and threshold adjustment. Journal of Business & Economic Statistics, 19(2), 166-176.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats AIC BIC na.omit embed residuals lm
#' @importFrom car linearHypothesis
#' @examples
#' x <- cumsum(rnorm(1000))
#' y <- cumsum(rnorm(1000))
#' MTAR_ECM(x, y, lags = 6)
#'
#' data(MarketPrices)
#' MTAR_ECM(MarketPrices[,1],MarketPrices[,2],lags = 2)
#'
MTAR_ECM<-function(y,x,lags){
y = as.vector(y)
x = as.vector(x)

res = residuals(lm(y~x))
z=diff(res)
gfz=embed(z,2)[,2]

k=lags+1
ygecikme = embed(diff(y),lags+1)[,2:k]
xgecikme = embed(diff(x),lags+1)[,2:k]

g1 = matrix(NA, nrow = lags+1, ncol = ncol(cbind(xgecikme)))
g2 = matrix(NA, nrow = lags+1, ncol = ncol(cbind(ygecikme)))

xf = rbind(g1,cbind(xgecikme))
yf = rbind(g2,cbind(ygecikme))
gfzesas = rbind(cbind(rep(NA, times=2)),cbind(gfz))
ybagli = rbind(cbind(rep(NA, times=1)),cbind(diff(y)))

dummy1 <- NULL
dummy1[gfz >= 0] = 0
dummy1[gfz < 0] = 1
dummy2 <- 1-dummy1

dummy1esas = rbind(cbind(rep(NA, times=2)),cbind(dummy1))
dummy2esas = rbind(cbind(rep(NA, times=2)),cbind(dummy2))
ECT1 = res*dummy1esas
ECT2 = res*dummy2esas
DX = xf
DY = yf
matris = na.omit(cbind(ybagli,ECT1,ECT2,DX,DY))
colnames(matris)[2] <- "ECT1"
colnames(matris)[3] <- "ECT2"

colnames(matris)[4] <- "DeltaX1"
colnames(matris)[4+lags] <- "DeltaY1"


MTAR_ECM = lm(ybagli~ECT1+ECT2+DX+DY)
my_list<-list("Model"=MTAR_ECM, "AIC" = AIC(MTAR_ECM),"BIC"=BIC(MTAR_ECM))
return(my_list)
}
