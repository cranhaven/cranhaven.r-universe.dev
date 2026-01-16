#' Plot Multiple Time Series in One Frame
#'
#' Plot multiple time series in one frame and return standardized time series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param title Character with the title of the plot. Default title is "mts plot".
#' @param scaling If scaling = TRUE (default), then each series is standardized based on its own range.
#' If scaling = FALSE, then the original series is used.
#' @param xtime A vector with the values for the x labels. Default values are 1, 2, 3, ...
#'
#' @return standardized - Matrix containing the standardized time series.
#'
#' @import graphics
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- mts.plot(TaiwanAirBox032017[,1:5])
#' @export
#'
"mts.plot" <- function(x, title = "mts plot", scaling = TRUE, xtime = NULL){

  if(!is.matrix(x))x <- as.matrix(x)
  k <- ncol(x)
  ran <- apply(x,2,range)
  sX <- x
  for (i in 1:k){
    sX[,i] <- (sX[,i]-ran[1,i])/(ran[2,i]-ran[1,i])
  }

  if(is.null(xtime))xtime <- c(1:nrow(x))
  if(scaling){
    plot(xtime,sX[,1],xlab='time',ylab='s-series',ylim=c(0,1),main=title,type="l")
    if(k > 1){
      for (i in 2:k){
        lines(xtime,sX[,i],col=i)
      }
    }
  }else{
    yy <- range(x)
    plot(xtime,x[,1],xlab='time',ylab='o-series',ylim=yy*1.02,main=title,type="l")
    if(k > 1){
      for (i in 2:k){
        lines(xtime,x[,i],col=i)
      }
    }
  }

  mts.plot <- list(standardized=sX)
}
