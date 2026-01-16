#' Generate Unit-root ARIMA Possibly Seasonal Time Series
#'
#' Generate Unit-root ARIMA, possibly, seasonal time series.
#'
#' @param T Number of observations.
#' @param ar Vector with the autoregressive coefficients. Default value is 0.5.
#' @param ma Vector with the moving average coefficients. Default value is -0.5.
#' @param d Order of first-differencing. Default value is 1.
#' @param sar Seasonal autoregressive coefficients. Default is NULL.
#' @param sma Seasonal moving average coefficients. Default is NULL.
#' @param D Order of seasonal differencing. Default value is 0.
#' @param period Seasonal period. Default value is 12.
#' @param ini Length of ‘burn-in’ period. Default value is 200.
#' @param df If df \eqn{\geq 50} random generation for the
#' Normal distribution, if df \eqn{< 50} random generation for the t distribution with df degrees
#' of freedom. Default value is 50.
#'
#' @return A time series vector.
#'
#' @examples
#' x <- sim.urarima()
#'
#' @export
"sim.urarima" <- function(T = 300, ar = c(0.5), ma = c(-0.5), d = 1, sar = NULL, sma = NULL, D = 0, period = 12, ini = 200, df = 50){

  n <- T
  dD <- max(d,D)
  if(dD > 0)ini <- 0

  if(period < 2)period=12

  nob <- n+ini+d+D*period
  if(df >= 50){
    at <- rnorm(nob)
  }else{at <- rt(nob,df=df)}

  ist <- 1

  if(length(sma) > 0){
    ist <- length(sma)*period+1
    tmp <- at[ist:nob]
    for (j in 1:length(sma)){
      tmp <- tmp - sma[j]*at[(ist-j*period):(nob-j*period)]
    }
  }
  a1t <- at
  if(ist > 1) a1t <- c(at[1:(ist-1)],tmp)

  if(length(ma) > 0){
    ist <- length(ma)+1
    tmp <- a1t[ist:nob]
    for (j in 1:length(ma)){
      tmp <- tmp - ma[j]*a1t[(ist-j):(nob-j)]
    }
    a1t[ist:nob] <- tmp
  }
  #
  s <- rep(0,period)
  s[period] <- 1
  x1t <- a1t

  if(length(ar) > 0) x1t <- filter(a1t,ar,method="r")
  if(length(sar) > 0){
    f1 <- kronecker(sar,s)
    x1t <- filter(x1t,f1,method="r")
  }
  if(d > 0){
    for (j in 1:d){
      x1t <- filter(x1t,c(1),method="r")
    }
  }
  if(D > 0)x1t <- filter(x1t,s,method="r")
  xt <- x1t[(ini+1):(ini+n)]

  xt
}
