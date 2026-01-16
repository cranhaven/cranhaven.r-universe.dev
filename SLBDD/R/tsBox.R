#' Boxplots of the Medians of Subperiods
#'
#' Find the median of each time series in the time span and
#' obtain the boxplots of the medians.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param maxbox Maximum number of boxes. Default value is 200.
#'
#' @return Boxplots of the medians of subperiods.
#'
#' @examples
#' data(TaiwanAirBox032017)
#' ts.box(as.matrix(TaiwanAirBox032017[,1:10]), maxbox = 10)
#' @export
"ts.box" <- function(x, maxbox = 200){

  if(!is.matrix(x))x <- as.matrix(x)
  nT <- nrow(x)
  if(maxbox > 200)maxbox <- 200
  if(maxbox < 1)maxbox <- 1
  nob <- floor(nT/maxbox)

  grpX <- NULL
  for (i in 1:maxbox){
    ist <- (i-1)*nob
    iend <- min(ist+nob,nT)
    grpmedian <- apply(x[(ist+1):iend,],2,median)
    grpX <- rbind(grpX,grpmedian)
  }
  grpX <- as.matrix(grpX)
  grpX <- t(grpX)

  boxplot(grpX,xlab="time index")
}
