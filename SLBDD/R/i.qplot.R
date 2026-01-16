#' Plot the Closest Series to a Given Timewise Quantile Series
#'
#' Use sum of absolute deviations to select the individual time series that is closest
#' to a given timewise quantile series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param prob Probability, the quantile series of which is to be computed.
#' Default value is 0.5.
#' @param box Number of boxplots for the difference series between the selected series and the
#' timewise quantile with the given probability. The differences are divided into blocks.
#' Default value is 3.
#' @param xtime A vector with the values for the x labels. Default values are 1, 2, 3, ...
#'
#' @return A list containing:
#' \itemize{
#' \item standardized - A matrix containing standardized time series.
#' \item qts - The timewise quantile of order prob.
#' \item selected - The closest time series to the given timewise quantile series.
#' }
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <-  i.qplot(TaiwanAirBox032017[,1:3])
#'
"i.qplot" <- function(x, prob = 0.5, box = 3, xtime = NULL){
  if(!is.matrix(x))x <- as.matrix(x)
  m1 <- mts.qplot(x,title="TS closest to a quantile series",prob=c(0.25,0.5,0.75),plot=FALSE)
  y <- m1$standardized; qseries <- m1$qseries
  nT <- nrow(y); k <- ncol(y)
  qts <- apply(y,1,quantile,prob=prob)
  ydif <- y-matrix(qts,nT,1)%*%matrix(rep(1,k),1,k)
  sabs <- apply(abs(ydif),2,sum)
  j <- which.min(sabs)
  if(is.null(xtime))xtime <- c(1:nT)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfcol=c(2,1))
  plot(xtime,qseries[,1],xlab='time',ylab='quantiles',main="Quantiles and nearest series",ylim=c(0,1),col=2,type="l")
  lines(xtime,qseries[,2],col=3)
  lines(xtime,qseries[,3],col=4)
  points(xtime,y[,j],pch="*",cex=0.5)
  lines(xtime,y[,j])
  size <- floor(nT/box)
  dis <- y[,j]-qts
  dist <- dis[1:size]
  if(box > 1){
    for (i in 2:box){
      ist <- (i-1)*size
      iend <- min(ist+size,nT)
      dist <- cbind(dist,dis[(ist+1):iend])
    }
  }
  dist <- as.matrix(dist)
  colnames(dist) <- paste("part",1:box,sep="")
  boxplot(dist,main="Boxplot of difference")

  return(list(standardized=y, qts=qts, selected=j))
}
