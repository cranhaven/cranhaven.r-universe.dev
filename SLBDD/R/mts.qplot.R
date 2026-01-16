#' Plot Timewise Quantiles in One Frame
#'
#' Plot timewise quantiles in one frame.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param title Character with the title of the plot. Default title is "mts quantile plot".
#' @param prob Probability, the quantile series of which is to be computed.
#' Default values are 0.25, 0.5, 0.75.
#' @param scaling If scaling = TRUE (default), then each series is standardized based on its own range.
#' If scaling = FALSE, then the original series is used.
#' @param xtime A vector with the values for the x labels. Default values are 1, 2, 3, ...
#' @param plot Receives TRUE or FALSE values. If the value is TRUE, a quantile plot is presented.
#' Defaults is TRUE.
#'
#' @return A list containing:
#' \itemize{
#' \item standardized - A matrix containing standardized time series.
#' \item qseries - Matrix of timewise quantiles series of order prob.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <-  mts.qplot(TaiwanAirBox032017[,1:5])
#'
#' @export
"mts.qplot" <- function(x, title = "mts quantile plot", prob = c(0.25,0.5,0.75), scaling = TRUE,
                        xtime = NULL, plot = TRUE){

  if(!is.matrix(x))x <- as.matrix(x)
  k <- ncol(x)
  ran <- apply(x,2,range)
  sX <- x
  for (i in 1:k){
    sX[,i] <- (sX[,i]-ran[1,i])/(ran[2,i]-ran[1,i])
  }
  nT <- nrow(sX)
  sX[is.na(sX)] <- 0
  kk <- length(prob)

  if(is.null(xtime))xtime <- c(1:nT)
  if(scaling){
    qseries <- apply(sX,1,quantile,prob=prob)
    yy <- range(sX)
  }else{
    qseries <- apply(x,1,quantile,prob=prob)
    yy <- range(x)
  }
  qseries <- t(qseries)

  if(plot){
    plot(xtime,qseries[,1],xlab='time',ylab='quantiles',main=title,ylim=yy*1.05,col=2,type="l")
    kk <- length(prob)
    if(kk > 1){
      for (j in 2:kk){
        lines(xtime,qseries[,j],col=(j+1))
      }
    }
  }

  return(list(standardized=sX, qseries=qseries))
}
