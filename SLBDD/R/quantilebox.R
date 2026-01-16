#' Quantile Boxplot
#'
#' Boxplots of selected quantiles of each time series (Column-wise operations).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param prob Probability, the quantile series of which is to be computed.
#' Default values are 0.25, 0.5, 0.75.
#'
#' @return Boxplot.
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' quantileBox(TaiwanAirBox032017[,1:3])
#'
"quantileBox" <- function(x, prob=c(0.25, 0.5, 0.75)){
  if(!is.matrix(x))x <- as.matrix(x)
  q <- apply(x,2,quantile,prob)
  mini <- apply(x,2,min)
  maxi <- apply(x,2,max)
  q <- cbind(mini,t(q),maxi)
  boxplot(q)
}
