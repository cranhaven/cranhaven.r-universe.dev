#' For a given periodically correlated multivariate process \code{X} eigendecompose it's spectral density
#' and use an inverse fourier transform to get coefficients of the optimal filters.
#'
#' @title Compute periodically correlacted DPCA filter coefficients
#' @param X multivariate stationary time series
#' @param period period of the periodic time series
#' @param q window for spectral density estimation as in \code{\link{spectral.density}}
#' @param freq frequency grid to estimate on as in \code{\link{spectral.density}}
#' @return principal components series
#' @references Kidzinski, Kokoszka, Jouzdani
#' Dynamic principal components of periodically correlated functional time series
#' Research report, 2016
#' @seealso \code{\link{pcdpca.inverse}}, \code{\link{pcdpca.scores}}
#' @examples
#' ## Prepare some process
#' library(fda)
#' library(freqdom)
#'
#' MSE = function(X,Y=0){ sum((X-Y)**2) / nrow(X) }
#'
#' d = 7
#' n = 100
#' A = t(t(matrix(rnorm(d*n),ncol=d,nrow=n))*7:1)
#' B = t(t(matrix(rnorm(d*n),ncol=d,nrow=n))*7:1)
#' C = t(t(matrix(rnorm(d*n),ncol=d,nrow=n))*7:1)
#'
#' X = matrix(0,ncol=d,nrow=3*n)
#' X[3*(1:n) - 1,] = A
#' X[3*(1:n) - 2,] = A + B
#' X[3*(1:n) ,] = 2*A - B + C
#'
#' basis = create.fourier.basis(nbasis=7)
#' X.fd = fd(t(Re(X)),basis=basis)
#' plot(X.fd)
#'
#' ## Hold out some datapoints
#' train = 1:(50*3)
#' test = (50*3) : (3*n)
#'
#' ## Static PCA ##
#' PR = prcomp(as.matrix(X[train,]))
#' Y1 = as.matrix(X) %*% PR$rotation
#' Y1[,-1] = 0
#' Xpca.est = Y1 %*% t(PR$rotation)
#'
#' ## Dynamic PCA ##
#' XI.est = dpca(as.matrix(X[train,]),
#'    q=3,
#'    freq=pi*(-150:150/150),
#'    Ndpc=1)  # finds the optimal filter
#' Y.est = freqdom::filter.process(X, XI.est$filters )
#' Xdpca.est = freqdom::filter.process(Y.est, t(rev(XI.est$filters)) )    # deconvolution
#'
#' ## Periodically correlated PCA ##
#' XI.est.pc = pcdpca(as.matrix(X[train,]),
#'    q=3,
#'    freq=pi*(-150:150/150),period=3)  # finds the optimal filter
#' Y.est.pc = pcdpca.scores(X, XI.est.pc)  # applies the filter
#' Y.est.pc[,-1] = 0 # forces the use of only one component
#' Xpcdpca.est = pcdpca.inverse(Y.est.pc, XI.est.pc)  # deconvolution
#'
#' ## Results
#' cat("NMSE PCA = ")
#' r0 = MSE(X[test,],Xpca.est[test,]) / MSE(X[test,],0)
#' cat(r0)
#' cat("\nNMSE DPCA = ")
#' r1 = MSE(X[test,],Xdpca.est[test,]) / MSE(X[test,],0)
#' cat(r1)
#' cat("\nNMSE PCDPCA = ")
#' r2 = MSE(X[test,],Xpcdpca.est[test,]) / MSE(X[test,],0)
#' cat(r2)
#' cat("\n")
#' @export
pcdpca = function(X,period=NULL,q=30,freq=(-1000:1000/1000) * pi){
  if (is.null(period))
    stop("You have to specify the period.")
  if (period <= 1)
    stop("Period must be greater or equal 2. Otherwise use the freqdom package.")
  dpc = freqdom::dpca(pc2stat(X,period=period),q=q,freq=freq)
  XI = dpc$filters
  XI$period = period
  XI
}
