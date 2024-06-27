% This is a template R document file
% Distributed under GPL 3 or later

\name{bw.dboot2}
\alias{bw.dboot2}
\title{A bootstrap bandwidth selection with resampling}
\description{
  To compute the optimal bandwidth using the bootstrap method with resampling.
}
\usage{
 bw.dboot2(y,sig,h0='dboot1',error='normal',B=1000,grid=100,ub=2)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviation(s) \eqn{\sigma}. For homoscedastic errors,  
    \eqn{sig} is a single value.  Otherwise, \eqn{sig} is a vector of variances 
    having the same length as \eqn{y}.}
  \item{h0}{An initial bandwidth parameter. The default vaule is the estimate from \link{bw.dboot1}. }
  \item{error}{Error distribution types: 'normal', 'laplacian'  for 
    normal and Laplacian errors, respectively.}
  \item{B}{Bootstrap number, default value 1000.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth 
   selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value 
    is "ub=2".}
}
\details{
	Three cases are supported: (1) homo normal; (2) homo laplacian.

	The integration was approximated by computing the average over a fine grid of points (1000 points).
}
\value{
the selected bandwidth.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
\code{\link{bw.dnrd}},  \code{\link{bw.dmise}},  \code{\link{bw.dboot1}}.
}
\examples{

n <- 1000
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
## the case of homoscedastic normal error
sig <- .8
u <- rnorm(n, sd=sig)
w <- x+u
bw.dboot2(w,sig=sig)
## the case of homoscedastic laplacian error
sig <- .8
## generate laplacian error
u <- ifelse(runif(n) > 0.5, 1, -1) * rexp(n,rate=1/sig)
w <- x+u
bw.dboot2(w,sig=sig,error='laplacian')

}
\keyword{smooth}

