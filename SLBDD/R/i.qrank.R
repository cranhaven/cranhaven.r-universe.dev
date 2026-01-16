#' Rank Individual Time Series According to a Given Timewise Quantile Series
#'
#' Use sum of absolute deviations to select the individual time series that is closest
#' to a given timewise quantile series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param prob Probability, the quantile series of which is to be computed.
#' Default value is 0.5.
#'
#' @return A list containing:
#' \itemize{
#' \item standardized - A matrix containing standardized time series.
#' \item qts - The timewise quantile of order prob.
#' \item ranks - Rank of the individual time series according to a the given timewise quantile series.
#' \item crit - Sum of absolute deviations of each individual series. Distance of each series to the quantile.
#' }
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <-  i.qrank(TaiwanAirBox032017[,1:3])
#'
"i.qrank" <- function(x, prob = 0.5){

  if(!is.matrix(x))x <- as.matrix(x)
  k <- ncol(x)
  ran <- apply(x,2,range)
  sX <- x
  for (i in 1:k){
    sX[,i] <- (sX[,i]-ran[1,i])/(ran[2,i]-ran[1,i])
  }
  y <- sX
  y[is.na(y)] <- 0
  nT <- nrow(y); k <- ncol(y)
  qts <- apply(y,1,quantile,prob=prob)
  ydif <- y-matrix(qts,nT,1)%*%matrix(rep(1,k),1,k)
  sabs <- apply(abs(ydif),2,sum)
  m2 <- sort(sabs,index.return=TRUE)
  rankidx <- m2$ix

  return(list(standardized=y,qts=qts,ranks = rankidx, crit=sabs))
}
