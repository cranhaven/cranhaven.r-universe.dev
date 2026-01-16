#' Find Outliers Using an Upper and a Lower Timewise Quantile Series
#'
#' Use an upper and a lower timewise quantile series to highlight the possible outliers
#' in a collection of  time series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param prob Tail probability. That is, the two quantile series is (prob, 1-prob).
#' prob is restricted to be in (0,0.15). Default value is 0.05.
#' @param percent The number of possible outliers in each side is T*k*prob*percent.
#' @param xtime A vector with the values for the x labels. Default values are 1, 2, 3, ...
#'
#' @return A list containing:
#' \itemize{
#' \item standardized - A matrix containing standardized time series.
#' \item qts - The timewise quantile of order prob.
#' \item minseries - The timewise minimum of the standardized time series.
#' \item maxseries - The timewise maximum of the standardized time series.
#' }
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <-  outlier.plot(TaiwanAirBox032017[,1:3])
#'
"outlier.plot" <- function(x, prob = 0.05, percent = 0.05, xtime = NULL){

  if(!is.matrix(x))x <- as.matrix(x)
  k <- ncol(x)
  ran <- apply(x,2,range)
  sX <- x
  for (i in 1:k){
    sX[,i] <- (sX[,i]-ran[1,i])/(ran[2,i]-ran[1,i])
  }
  y <- sX
  qts <- NULL
  nT <- nrow(y); k <- ncol(y)
  if(is.null(xtime))xtime <- c(1:nT)
  if(prob <= 0) prob <- 0.05
  if(prob >= 0.15) prob <- 0.05
  pr <- c(prob, 1-prob)
  qts <- apply(y,1,quantile,prob=pr)
  qts <- t(qts)
  minser <- apply(y,1,min)
  maxser <- apply(y,1,max)
  plot(xtime,qts[,1],xlab='time',ylab='s-series',ylim=c(0,1),type="l")
  lines(xtime,qts[,2],col=1)
  d1 <- qts[,1]-minser
  d2 <- maxser-qts[,2]
  d11 <- sort(d1,index.return=TRUE)
  d22 <- sort(d2,index.return=TRUE)
  npts <- max(floor(nT*k*prob*percent),5)
  idx <- d11$ix[(nT-npts+1):nT]
  jdx <- d22$ix[(nT-npts+1):nT]
  points(xtime[idx],minser[idx],pch="o",cex=0.6,col="blue")
  points(xtime[jdx],maxser[jdx],pch="o",cex=0.6,col="red")

  return(list(standardized=y,qts=qts,minseries=minser,maxseries=maxser))

}
