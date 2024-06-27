% This is a template R document file
% Distributed under GPL 3 or later

\name{npdenest}
\alias{npdenest}

\title{Estimating probability density function from data with unknown
measurement error}

\description{ To compute the probability density function from data
  contaminated with measurement error. The measurement error type is
  unknown.  }

\usage{

 npdenest(w, e, bw, adjust = 1, n = 512, from, to, cut = 3, na.rm = FALSE,...) 

}
\arguments{
  \item{w}{The observed data.  It is a vector of length at least 3.}
  \item{e}{Observed vector of measurement errors. }
  \item{bw}{Smoothing parameter.}
  \item{adjust}{adjust the range there the PDF is to be evaluated.  
  By default, \eqn{adjust=1}.}
  \item{n}{number of points where the PDF is to be evaluated.}
  \item{from}{the starting point where the PDF is to be evaluated.}
  \item{to}{the starting point where the PDF is to be evaluated.}
  \item{cut}{used to adjust the starting end ending points where the 
  PDF is to be evaluated.}
  \item{na.rm}{ is set to FALSE by default: no NA value is allowed.}
  \item{...}{controls}
}

\details{ The optimal bandwidth is selected by minimizing
  \code{abs(Var(f.hat)+Var(E)-Var(Y))}.  } \value{ An object of class
  ``Decon''.  }

\references{
 Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}

\seealso{
 \code{\link{DeconCdf}}, \code{\link{DeconNpr}}, \code{\link{DeconCPdf}}.
}

\examples{

 n1 <- 1500
 x1 <- rnorm(n1, sd=1)
 sig1 <- .5
 u1 <- ifelse(runif(n1) > 0.5, 1, -1) * rexp(n1,rate=1/sig1)
 w1 <- x1+u1
 ## The rule-of-thumb method may not be accurate, 
 ## you may try the bootstrap method
 bw1 <- bw.dnrd(w1,sig=sig1, error="laplacian")
 (f1 <-  DeconPdf(w1,sig1,error='laplacian',bw=bw1, fft=TRUE))
 (f2 <-  npdenest(w1, u1))

 # plot the results
 par(mfrow=c(1,1))
 plot(f1,  col="red", lwd=3, lty=2, xlab="x", ylab="f(x)", main="")
 lines(density(x1, from=min(w1), to=max(w1)), lwd=3, lty=1)
 lines(density(w1), col="blue", lwd=3, lty=3)
 lines(f2, col='red', lty=1,lwd=3)

}
\keyword{nonparametric}
\keyword{smooth}
\keyword{density estimation}
\keyword{measurement error}

