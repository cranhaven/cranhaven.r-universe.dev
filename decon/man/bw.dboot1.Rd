% This is a template R document file
% Distributed under GPL 3 or later

\name{bw.dboot1}
\alias{bw.dboot1}
\title{A bootstrap bandwidth selection without resampling}
\description{
  To compute the optimal bandwidth using the bootstrap-type method without generation of any bootstrap sample.
}
\usage{
 bw.dboot1(y,sig, h0="dnrd", error="normal", grid=100, ub=2)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviation(s) \eqn{\sigma}. For homoscedastic errors,  
    \eqn{sig} is a single value.  Otherwise, \eqn{sig} is a vector of variances 
    having the same length as \eqn{y}.}
  \item{h0}{An initial bandwidth parameter. The default vaule is the estimate from \link{bw.dnrd}. }
  \item{error}{Error distribution types: 'normal', 'laplacian'  for 
    normal and Laplacian errors, respectively.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth 
   selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value 
    is "ub=2".}
}
\details{
Three cases are supported: (1) homo normal; (2) homo laplacian; (3) hetero normal.

Case (3) could be very slow, we reduce the number of grid points in computing the L-2 distance to 100 and reduce the optimal bandwidth searching grid points to 50 to speed up the algorithm.

The integration was approximated by computing the average over a fine grid of points (1000 points).

The case of heteroscedastic laplacian errors is not supported and is to be developed. 
}
\value{
the selected bandwidth.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Delaigle, A. and Gijbels, I. (2004). Practical bandwidth selection in
deconvolution kernel density estimation. \emph{Computational Statistics and Data
Analysis}, 45, 249-267.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{bw.dnrd}}, \code{\link{bw.dmise}}, \code{\link{bw.dboot2}}.
}
\examples{

n <- 1000
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
## the case of homoscedastic normal error
sig <- .8
u <- rnorm(n, sd=sig)
w <- x+u
bw.dboot1(w,sig=sig)
## the case of homoscedastic laplacian error
sig <- .8
## generate laplacian errors
u <- ifelse(runif(n) > 0.5, 1, -1) * rexp(n,rate=1/sig)
w <- x+u
bw.dboot1(w,sig=sig,error='laplacian')
## the case of heteroscedastic normal error
sig <- runif(n, .7, .9)
u <- sapply(sig, function(x) rnorm(1, sd=x))
w <- x+u
bw.dboot1(w,sig=sig,error='normal')


}
\keyword{smooth}

