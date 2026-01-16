#' Summary Statistics of Cross-Correlation Matrices
#'
#' Compute and plot summary statistics of cross-correlation matrices (CCM) for high-dimensional time series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param max.lag The number of lags for CCM.
#'
#' @return A list containing:
#' \itemize{
#' \item pvalue - P-values of Chi-square tests of individual-lag CCM being zero-matrix.
#' \item ndiag - Percentage of significant diagonal elements for each lag.
#' \item noff - Percentage of significant off-diagonal elements for each lag.
#' }
#' @export
#'
#' @importFrom MTS ccm
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- Summaryccm(as.matrix(TaiwanAirBox032017[,1:4]))
#'
"Summaryccm" <- function(x, max.lag = 12){

  if(!is.matrix(x)){
    message("Data must be in T-by-k matrix format!","\n")
  }else{
    k <- ncol(x)
    nT <- nrow(x)
    ksq <- k*k
    m1 <- MTS::ccm(x,lags=max.lag,level=FALSE,output=FALSE)
    pvalue <- m1$pvalue
    ccm <- m1$ccm
    crit <- 2/sqrt(nT)
    ndiag <- rep(0,max.lag)
    noff <- rep(0,max.lag)
    for (i in 1:max.lag){
      c1 <- c(ccm[,(i+1)])
      idx <- c(1:ksq)[abs(c1) >= crit]
      noff[i] <- length(idx)
      cmtx <- matrix(c1,k,k)
      idx <- c(1:k)[abs(diag(cmtx)) >= crit]
      ndiag[i] <- length(idx)
      noff[i] <- noff[i]-ndiag[i]
    }

    ndiag <- ndiag/k
    noff <- noff/(ksq-k)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfcol=c(3,1))
    plot(1:max.lag,pvalue,type="h",xlab="lag",ylab="p-value",ylim=c(-0.05,1.05))
    points(1:max.lag,pvalue,pch="o")
    abline(h=c(crit),col="blue")
    abline(h=c(0))
    #
    plot(1:max.lag,ndiag,type="l",xlab="lag",ylab="diagonal",ylim=c(-0.05,1.05))
    points(1:max.lag,ndiag,pch="o")
    plot(1:max.lag,noff,type="l",xlab="lag",ylab="off-diagonal",ylim=c(-0.050,1.05))
    points(1:max.lag,noff,pch="o")
  }

  return(list(pvalue = pvalue, ndiag = ndiag, noff = noff))
}
