#' Contour plot for the kernels of cross-covariance operators.
#' 
#' @title Contour plot for the kernels of cross-covariance operators.
#' 
#' @param X an object of class \code{\link[fda]{fd}} representing a functional data sample.
#' @param Y an object of class\code{\link[fda]{fd}} representing a functional data sample.
#' @param cor if \code{FALSE} covariance kernels are plotted, if \code{TRUE} correlation kernel will be plotted.
#' @param res number of discretization points to evaluate functional data.
#' @param lags lags to plot, dafauts \code{0:3}
#' @param nlevels number of color levels for the contour plot.
#' @export
#' @keywords plotting
#' @examples 
#' fts = fts.rar(100)
#' 
#' # Plot covariance operators of the time series curves
#' # We chose resolution equal 150 for better precision 
#' fts.plot.covariance(fts, lags=0:2, res = 150) 
#' 
#' # Plot correlation operators of the time series curves
#' fts.plot.covariance(fts, lags=0:2, cor = TRUE, res = 50)
#' 
#' # Make the grid of levels more dense
#' fts.plot.covariance(fts, lags=0:1, nlevels = 100)
fts.plot.covariance = function(X, Y = X, cor = FALSE, res=200, lags = 0:3, nlevels=25){
  if(cor==TRUE){
    X=sd1(X)
    Y=sd1(Y)	
  }
  A=fts.cov.structure(X,Y,lags=lags)	
  fts.plot.operators(A,lags=lags,res=res,nlevels=nlevels)
}