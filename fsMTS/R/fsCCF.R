fsCCF <- function(mts, max.lag,  show.progress = TRUE) {
  n <- ncol(mts)
  res <- matrix(0, n*max.lag, n)
  for (i in 1:n){
    for (j in i:n){
        corVals <- stats::ccf(mts[,i],mts[,j], lag.max=max.lag, plot=F, na.action = stats::na.pass)
        res[j+(0:(max.lag-1))*n, i] <- rev(corVals$acf[1:max.lag])
        res[i+(0:(max.lag-1))*n, j] <- corVals$acf[(max.lag+2):(2*max.lag+1)]
    }
    if (show.progress) svMisc::progress(100*i/n)
  }
  res <- fsNames(res, mts, max.lag)
  res <- abs(res)
  return (res)
}
