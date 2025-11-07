#' Plot kernels
#'
#' @param A a functional filter sequence given as object of class \code{\link{fts.timedom}}.
#' @param Ndpc if Ndpc = k the first k filter sequences are plotted.
#' @param lags number of lags to plot.
#' @param one.plot if TRUE then functional filters corresponding belonging to the respective scores will all be plotted in the same graph.
#' @param ... arguments \code{col, lwd, lty} passed to \code{plot}
#' @keywords plotting
#' @importFrom stats update
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline filled.contour legend lines plot text title
#' @export
#' @examples
#' # Load example PM10 data from Graz, Austria
#' data(pm10) # loads functional time series pm10 to the environment
#' X = center.fd(pm10)
#'
#' # Compute functional dynamic principal components with only one component
#' res.dpca = fts.dpca(X, Ndpc = 1, freq=(-25:25/25)*pi) # leave default freq for higher precision
#' 
#' # Plot Functional Dynamic Principal Component Filters
#' fts.plot.filters(res.dpca$filters)
fts.plot.filters = function(A, Ndpc = 1, lags = -3:3, one.plot=FALSE,...)
{
  A = fts.timedom.trunc(A, lags)
  d = dim(A$operators)[3]

  # check if lags are in consecutive order
  if (any(A$lags[2:d] - A$lags[1:(d-1)] != 1) && length(lags) > 1)
    stop("lags must be in consecutive order")

  cmin = 1e2
  cmax = -1e2
  
  # defaults
  cols = 1:Ndpc
  lwd = 2
  lty = 1

  arg <- list(...)
  if ("col" %in% names(arg))
    cols = rep(arg[["col"]], Ndpc)
  if ("lwd" %in% names(arg))
    lwd = arg[["lwd"]]
  if ("lty" %in% names(arg))
    lty = arg[["lty"]]
  arg$lwd=NULL
  arg$lty=NULL
  arg$col=NULL

  lo=A$basisX$rangeval[1]
  up=A$basisX$rangeval[2]
  grid = lo+(up-lo)*0:100/100
  grid01 = 0:100/100

  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(A$operators[dpc,,i]),A$basisX)
      evals = eval.fd(grid,F)
      if (cmin > min(evals)) cmin = min(evals)
      if (cmax < max(evals)) cmax = max(evals)
    }
  }
  
  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(A$operators[dpc,,i]),A$basisX)
      evals = eval.fd(grid,F)
      if (i == 1 && (dpc == 1 || !one.plot)){
        xlim = c(A$lags[1],A$lags[d] + 1)
        do.call(function(...) { plot(grid01 + A$lags[1],evals,xlim=xlim,ylim=c(cmin,cmax),xlab="lags", ylab="",xaxt='n',col=cols[dpc],lwd = lwd,lty = lty,type = 'l',bty="n",...) }, arg)
      }
      else {
        do.call(function(...) { lines(grid01 + A$lags[i],evals, col=cols[dpc], lwd = lwd, lty = lty,...) }, arg)
      }
      text(0.5  + A$lags[i], cmin - (cmax - cmin)*0.02,labels= A$lags[i] )
      if (i == 1)
        abline(h=0,lty=1)
      if (i < d)
        abline(v=A$lags[i]+1,lty=2,col="darkgrey")
    }
    if (!one.plot)
      title(paste("Filter",dpc))
  }
  if (one.plot){
    legend(A$lags[1],cmax,paste("filter",1:Ndpc), 
           lty=c(1,1),
           lwd=c(2.5,2.5), col = cols) 
    title(paste("Components"))
  }
}

