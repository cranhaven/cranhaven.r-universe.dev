#' Contour plot of operator kernels.
#' 
#' @title Contour plot of operator kernels.
#' 
#' @param A an object of class \code{fts.timedom} or \code{\link{fts.freqdom}}.
#' @param res number of discretization points to evaluate functional data.
#' @param lags a vector of integers. For objects of class \code{\link{fts.timedom}}
#' the lags of the operators we want to plot.
#' @param freq a vector of frequencies in \eqn{[-\pi,\pi]}. For an object of class
#' \code{fts.freqdom} the frequencies at which we want to plot the operator. If the
#' chosen frequencies are not contained in \code{A$freq}, the closest frequencies will be used.
#' @param axis if \code{"Re"} we plot the real part, if \code{"Im"} we plot the imaginary part of a complex-valued operator.
#' @param nlevels number of color levels for the contour plot.
#' @export
#' @keywords plotting
#' @examples
#' # Load example PM10 data from Graz, Austria
#' data(pm10) # loads functional time series pm10 to the environment
#' X = center.fd(pm10)
#'
#' # Compute functional dynamic principal components with only one component
#' res.dpca = fts.dpca(X, Ndpc = 1, freq=(-25:25/25)*pi) # leave default freq for higher precision
#' 
#' # Plot the spectral density operator at frequencies -2, -3:3/30 * pi and 2
#' fts.plot.operators(res.dpca$spec.density,freq = c(-2,-3:3/30 * pi,2))
fts.plot.operators = function(A, res=200, lags = 0, freq = 0, axis = "Re", nlevels = 25){
  lags.label="lags"
  if (is.fts.timedom(A)){
    A = fts.timedom.trunc(A,lags = lags)
    lags = A$lags
  }
  if (is.fts.freqdom(A)){
    # select frequencies close to desired ones
    nfreq = length(freq)
    dist = abs(t(A$freq%*% t(rep(1,nfreq))) - freq)
    mins = apply(dist,1,which.min)
    
    # truncate the operator
    A$operators = A$operators[,,mins,drop=FALSE]
    A$freq = A$freq[mins]
    lags = A$freq
    lags.label="frequencies"
  }
  nlags = dim(A$operators)[3]
  
  basis1=A$basisX
  basis2=A$basisY
  z=c()
  int1=basis1$rangeval[1]+(basis1$rangeval[2]-basis1
                           $rangeval[1])*1:res/res
  int2=basis2$rangeval[1]+(basis2$rangeval[2]-basis2$rangeval[1])*1:res/res
  for(i in 1:nlags){	
    M = Re(A$operators[,,i])
    if (axis == "Im")
      M = Im(A$operators[,,i])
    
    Afun=bifd (coef=M, sbasisobj=basis1,
               tbasisobj=basis2)
    z=rbind(z,eval.bifd(int1,int2, Afun))
  }	
  annotations = function(){
    for(i in 1:(nlags-1)){	
      abline(v=i,lty=2,col="black")
    }
    if (is.fts.freqdom(A))
      lags = format(round(lags, 2), nsmall = 2)
    axis(1, at=1:nlags - 0.5, tick = FALSE, labels = lags) # axis(2)
  }
  
  filled.contour(1:(res*nlags)/res, int2, z, color.palette=colorRampPalette(c("blue", "white", "red"), space="rgb"),
                 nlevels=nlevels,zlim=c(-max(abs(z)),max(abs(z))),
                 plot.axes = { annotations() },
                 main="contour of kernels",xlab=lags.label,xaxt = "n")
}


sd1<-function(X){
  X0.f=center.fd(X)
  scale.f=sd.fd(X)
  int=X$basis$rangeval
  X0.d=eval.fd(int[1]+(1:100/100)*(int[2]-int[1]),X0.f)
  scale.d=eval.fd(int[1]+(1:100/100)*(int[2]-	int[1]),scale.f)
  for(i in 1:dim(X0.d)[2]){
    X0.d[,i]=X0.d[,i]/scale.d
  }
  fd(X0.d,create.bspline.basis(nbasis=100))
}

