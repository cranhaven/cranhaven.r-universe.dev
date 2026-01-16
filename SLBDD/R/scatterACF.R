#' Scatterplot of Two Selected-lag Autocorrelation Functions
#'
#' Scatterplot of two selected-lag ACFs.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param lags Set of lags. Default values are 1, 2.
#'
#' @return A list containing:
#' \itemize{
#' \item acf1 - Autocorrelation function of order lags[1].
#' \item acf2 - Autocorrelation function of order lags[2].
#' }
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- scatterACF(TaiwanAirBox032017[,1:100])

"scatterACF" <- function(x, lags = c(1,2)){
  if(!is.matrix(x))x <- as.matrix(x)
  x1 <- scale(x,center=TRUE,scale=TRUE)
  if(min(lags) < 1)lags=c(1,2)
  max.lag <- max(lags)
  ist <- max.lag+1
  nT <- nrow(x)
  c1 <- crossprod(x1[ist:nT,],x1[(ist-lags[1]):(nT-lags[1]),])/nT
  c2 <- crossprod(x1[ist:nT,],x1[(ist-lags[2]):(nT-lags[2]),])/nT
  plot(diag(c2),diag(c1),xlab="Lag-2 ACF",ylab="Lag-1 ACF",pch="*")

  return(list(acf1=diag(c1), acf2=diag(c2)))
}
