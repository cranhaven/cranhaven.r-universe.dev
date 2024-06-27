% This is a template R document file
% Distributed under GPL 3 or later

\name{DeconPdf}
\alias{DeconPdf}
\title{Estimating probability density function from data with measurement error}
\description{
  To compute the probability density function from data contaminated with measurement error. The measurement errors can be either homoscedastic or heteroscedastic.
}
\usage{
DeconPdf(y,sig,x,error='normal',bw='dboot1',adjust=1,fft=FALSE,
	n=512,from,to,cut=3,na.rm=FALSE,grid=100,ub=2, ...)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviations \eqn{\sigma}.  If homoscedastic errors, \eqn{sig} is a single value.
  If heteroscedastic errors,  \eqn{sig} is a vector of standard deviations having the same length 
  as \eqn{y}.}
  \item{x}{x is user-defined grids where the PDF will be evaluated. FFT method is not applicable if x is given. }
  \item{error}{Error distribution types: (1) 'normal' for normal errors; (2) 'laplacian' for Laplacian errors; (3) 'snormal' for a special case of small normal errors.}
  \item{bw}{Specifies the bandwidth. It can be a single numeric value which has been pre-determined; or computed with the specific bandwidth selector: 'dnrd' to compute the rule-of-thumb plugin bandwidth as suggested by Fan (1991); 'dmise' to compute the plugin bandwidth by minimizing MISE; 'dboot1' to compute the bootstrap bandwidth selector without resampling (Delaigle and Gijbels, 2004a), which minimizing the MISE bootstrap bandwidth selectors; 'boot2' to compute the smoothed bootstrap bandwidth selector with resampling.}
  \item{adjust}{adjust the range there the PDF is to be evaluated.  By default, \eqn{adjust=1}.}
  \item{fft}{To specify the method to compute the PDF.  'fft=FALSE' to compute directly; 'fft=TRUE' to compute the PDF by using the Fast Fourier Transformation.}
  \item{n}{number of points where the PDF is to be evaluated.}
  \item{from}{the starting point where the PDF is to be evaluated.}
  \item{to}{the starting point where the PDF is to be evaluated.}
  \item{cut}{used to adjust the starting end ending points where the PDF is to be evaluated.}
  \item{na.rm}{ is set to FALSE by default: no NA value is allowed.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value is
 "ub=2".}
  \item{...}{control }
}
\details{
  If the number of points to be evaluated is too small (less than 32), a direct computing method is preferred.  The current version can support up to \eqn{2^21} points where the PDF to be computed. 
}
\value{
An object of class ``Decon''.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Delaigle, A. and Meister, A. (2008). Density estimation with heteroscedastic error. \emph{Bernoulli}, 14, 562-579.

Fan, J. (1991). On the optimal rates of convergence for nonparametric deconvolution problems. \emph{The Annals of Statistics}, 19, 1257-1272. 

Fan, J. (1992). Deconvolution with supersmooth distributions. \emph{The Canadian Journal of Statistics}, 20, 155-169.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{DeconCdf}}, \code{\link{DeconNpr}}, \code{\link{DeconCPdf}}.
}
\examples{

## Deconvolution: the case of homoscedastic errors
## Case 1.1: homoscedastic Laplacian errors
n1 <- 500
x1 <- rnorm(n1, sd=1)
sig1 <- .5
u1 <- ifelse(runif(n1) > 0.5, 1, -1) * rexp(n1,rate=1/sig1)
w1 <- x1+u1
## The rule-of-thumb method may not be accurate, 
## you may try the bootstrap method
bw1 <- bw.dnrd(w1,sig=sig1, error="laplacian")
f1 <-  DeconPdf(w1,sig1,error='laplacian',bw=bw1, fft=TRUE)

## Case 1.2: homoscedastic normal errors
#n2 <- 1000
#x2 <- c(rnorm(n2/2,-3,1),rnorm(n2/2,3,1))
#sig2 <- .8
#u2 <- rnorm(n2, sd=sig2)
#w2 <- x2+u2
# estimate the bandwidth with the bootstrap method with resampling
#bw2 <- bw.dboot2(w2,sig=sig2, error="normal")
# estimate the unknown density with measurement error
#(f2 <-  DeconPdf(w2,sig2,error='normal',bw=bw2, fft=TRUE))

# plot the results
#par(mfrow=c(1,2))
#plot(f1,  col="red", lwd=3, lty=2, xlab="x", ylab="f(x)", main="")
#lines(density(x1, from=min(w1), to=max(w1)), lwd=3, lty=1)
#lines(density(w1), col="blue", lwd=3, lty=3)
#plot(f2,  col="red", lwd=3, lty=2, xlab="x", ylab="f(x)", main="")
#lines(density(x2, from=min(w2), to=max(w2)), lwd=3, lty=1)
#lines(density(w2), col="blue", lwd=3, lty=3)


## Deconvolution: the case of heteroscedastic errors
## Case 2: heteroscedastic normal errors
#n3 <- 2000
#x3 <- rchisq(n3, df=1.5, ncp=0)
#sig3 <- 0.7+ x3/max(x3)
#u3 <- sapply(sig3, function(x) rnorm(1, sd=x))
#w3 <- x3+u3
# estimate the bandwidth using the bootstrap method withou resampling
#bw3 <- bw.dboot1(w3,sig=sig3, error="normal")
# estimate the unknown density with measurement error
#(f3 <-  DeconPdf(w3,sig3,error="normal", bw=bw3, fft=TRUE))

# plot the results
#par(mfrow=c(1,1))
#plot(f3,  col="red", lwd=3, lty=2, ylim=c(0,0.4), xlab="x", ylab="f(x)", main="")
#lines(density(x3, adjust=2), lwd=3, lty=1)
#lines(density(w3, adjust=2), col="blue", lwd=3, lty=3)


}
\keyword{nonparametric}
\keyword{smooth}
\keyword{density estimation}
\keyword{measurement error}

