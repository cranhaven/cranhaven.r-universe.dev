% This is a template R document file
% Distributed under GPL 3 or later

\name{bw.dnrd}
\alias{bw.dnrd}
\title{A rule of thumb bandwidth selection in denconvolution problems}
\description{
  To compute the optimal bandwidth using the rule-of-thumb methods 
 based on theorem 1 and theorem 2 of Fan (1991).
}
\usage{
 bw.dnrd(y,sig,error='normal')
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviation(s) \eqn{\sigma}. For homoscedastic errors,  \eqn{sig} is a single value.
  otherwise, \eqn{sig} is a vector of variances having the same length 
  as \eqn{y}.}
  \item{error}{Error distribution types: 'normal', 'laplacian'  for 
    normal and Laplacian errors, respectively.}
}
\details{
The current version approximate the 
    second term in the MISE by assuming that \eqn{X} is 
  normally distributed. In the case of heteroscedastic error, the variance was approximated by the arithematic mean of the variances of \eqn{U}.
}
\value{
the selected bandwidth.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Fan, J. (1991). On the optimal rates of convergence for nonparametric deconvolution problems. \emph{The Annals of Statistics}, 19, 1257-1272.
    
Fan, J. (1992). Deconvolution with supersmooth distributions. \emph{The Canadian Journal of Statistics}, 20, 155-169.   
    
Stefanski, L. and Carroll, R. J. (1990). Deconvoluting kernel density estimators. \emph{Statistics}, 21, 169-184.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
    }
\seealso{
 \code{\link{bw.dmise}}, \code{\link{bw.dboot1}}, \code{\link{bw.dboot2}}.
}
\examples{
n <- 1000
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
## the case of homoscedastic normal error
sig <- .8
u <- rnorm(n, sd=sig)
w <- x+u
bw.dnrd(w,sig=sig)
## the case of homoscedastic laplacian error
sig <- .8
## generate laplacian errors
u <- ifelse(runif(n) > 0.5, 1, -1) * rexp(n,rate=1/sig)
w <- x+u
bw.dnrd(w,sig=sig,error='laplacian')
## the case of heteroscedastic normal error
sig <- runif(n, .7, .9)
u <- sapply(sig, function(x) rnorm(1, sd=x))
w <- x+u
bw.dnrd(w,sig=sig,error='normal')
}
\keyword{smooth}

