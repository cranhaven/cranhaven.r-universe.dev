#' Plot a Selected Time Series Using Quantile as the Background
#'
#' Plot a selected time series using quantile as the background.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param idx Selected time series.
#' @param prob Probability, the quantile series of which is to be computed.
#' Default values are 0.25, 0.5, 0.75.
#' @param xtime A vector with the values for the x labels. Default values are 1, 2, 3, ...
#'
#' @return standardized - Matrix containing the standardized time series.
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- i.plot(TaiwanAirBox032017[,1:3])
#'
#' @export
"i.plot" <- function(x, idx = 1, prob = c(0.25,0.5,0.75), xtime = NULL){

  if(!is.matrix(x))x <- as.matrix(x)
  if(idx < 1)idx=1
  if(idx > ncol(x))idx <- ncol(x)
  if(is.null(xtime))xtime <- c(1:nrow(x))
  m1 <- mts.qplot(x,title="Selected time series in quantile plot",scaling=TRUE,prob=prob,xtime=xtime)
  y <- m1$standardized
  points(xtime,y[,idx],pch="*",cex=0.4)
  lines(xtime,y[,idx])
  i.plot <- list(standardized=y)
}
