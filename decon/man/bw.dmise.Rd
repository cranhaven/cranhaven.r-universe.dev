% This is a template R document file
% Distributed under GPL 3 or later

\name{bw.dmise}
\alias{bw.dmise}
\title{The MISE based plug-in bandwidth selection}
\description{
  To compute the optimal bandwidth using the plug-in methods by minimizing MISE.
}
\usage{
 bw.dmise(y,sig,error="normal",kernel="support",grid=100,ub=2)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviation(s) \eqn{\sigma}. For homoscedastic errors,  
    \eqn{sig} is a single value.  Otherwise, \eqn{sig} is a vector of variances 
    having the same length as \eqn{y}.}
  \item{error}{Error distribution types: 'normal', 'laplacian'  for 
    normal and Laplacian errors, respectively.}
  \item{kernel}{Kernel type: 'support' for support kernel; 
  and 'normal' for Gaussian kernel.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth 
   selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value 
    is "ub=2".}
}
\details{
The current version approximate the 
    second term in the MISE by assuming that \eqn{X} is 
  normally distributed.}
\value{
the selected bandwidth.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Fan, J. (1992). Deconvolution with supersmooth distributions. 
\emph{The Canadian Journal of Statistics}, 20, 155-169.   

Stefanski, L. and Carroll, R. J. (1990). Deconvoluting kernel density estimators. 
\emph{Statistics}, 21, 169-184.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{bw.dnrd}}, \code{\link{bw.dboot1}}, \code{\link{bw.dboot2}}.
}
\examples{

n <- 1000
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
## the case of homoscedastic normal error
sig <- .8
u <- rnorm(n, sd=sig)
w <- x+u
bw.dmise(w,sig=sig,error='normal');
## The small error case
sig <- .25
u <- rnorm(n, sd=sig)
w <- x+u
bw.dmise(w,sig=sig,kernel='normal',error='normal');

## the case of homoscedastic laplacian error
sig <- .8
## generate laplacian error
u <- ifelse(runif(n) > 0.5, 1, -1) * rexp(n,rate=1/sig)
w <- x+u
bw.dmise(w,sig=sig,error='laplace')

## the case of heteroscedastic normal error
sig <- runif(n, .7, .9)
u <- sapply(sig, function(x) rnorm(1, sd=x))
w <- x+u
bw.dmise(w,sig=sig,kernel='support',error='normal')

}
\keyword{smooth}

