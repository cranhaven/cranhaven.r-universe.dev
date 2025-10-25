#'
#' Plot sample size dependent AUC or MCC based on number of selected features
#'
#' Plot the output returned by samplesize function
#'
#' @method plot planningML
#'
#' @param x the output returned by the samplesize function
#' @param ... ignored arguments
#'
#' @importFrom graphics legend lines title
#' @return \code{plot()} returns a scatterplot of sample size dependent performance measurement metrics (AUC or MCC) based on number of selected features
#' @export


plot.planningML = function(x, ...){
  if (!is.null(x$outtable)){
    sample.size = x$samplesize
    tbl = x$outtable
    m = x$m
    # num_m = length(m)
    metric = x$metric

    plot(sample.size, tbl[,1], type="b", col="red", lwd=1, pch=10, xlab="Sample.size(n)", ylab=paste(metric,"(n)",sep=""),ylim=range(0,1))
    pchlist = 10
    if (length(m) > 1){
      pchlist = c(10)
      for (i in 2:length(m)){
        lines(sample.size, tbl[,i],type="b", col="red", lwd=2, pch=(10 + 2*i))
        pchlist = c(pchlist, 10+2*i)
      }
    }
    title("Sample Size Determination")
    legend(200, 0.99, legend=paste("m=",m,sep=""),
           col=c("red"), lty=1, cex=0.6, pch = pchlist,
           box.lty=1, box.lwd=2, box.col="black")
  }

  if (!is.null(x$predY)){

    plot(x$x, x$predY, type="l", lwd=1, xlab="Sample size", ylab=x$metric, ylim = c(0,2))
    lines(x$x, x$predY.lw, type = "l", lty=2, col="blue")
    lines(x$x, x$predY.up, type = "l", lty=2, col="blue")
  }


}
