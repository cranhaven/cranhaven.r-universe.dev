% This is a template R document file
% Distributed under GPL 3 or later

\name{npreg}
\alias{npreg}

\title{Nonparametric regression based on data with unknown
measurement error}

\description{ To compute nonparametric regression line from data
  contaminated with measurement error. The measurement error type is
  unknown.  }

\usage{

 npreg(w, y, e, bw, adjust = 1, n = 512, from, to, cut = 0, na.rm = FALSE,...) 

}
\arguments{
  \item{w,y}{The observed data \code{W} and \code{Y}.  
  It is a vector of length at least 3.}
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

\references{ Wang, X.F. and Wang, B. (2011). Deconvolution estimation
 in measurement error models: The R package decon. \emph{Journal of
 Statistical Software}, 39(10), 1-24.  }

\seealso{
 \code{\link{DeconCdf}}, \code{\link{DeconNpr}}, \code{\link{DeconCPdf}}.
}

\examples{

n <- 2000
x <- c(rnorm(n/2,2,1), rnorm(n/2, -2,1))
sig <- .8
u <- sig*rnorm(n)
w <- x+u
e <- rnorm(n, sd=0.2)
y <- x^2-2*x+e
bw1 <- bw.dboot1(w, sig)
u0 <- sig*rnorm(n/2) # typically the size of u0 is smaller than x.
m2 <- npreg(w, y, u0, from=0.9*min(x), to=0.9*max(x))

# plot the results
plot(m2, col="red", lwd=3, lty=2, xlab="x", ylab="m(x)", main="", 
			zero.line=FALSE)
lines(ksmooth(x,y, kernel = "normal", 2, range.x=c(0.9*min(x),0.9*max(x))), 
			lwd=3, lty=1)
lines(ksmooth(w,y, kernel = "normal", 2, range.x=c(0.9*min(x),0.9*max(x))), 
			col="blue", lwd=3, lty=3)

}
\keyword{nonparametric}
\keyword{smooth}
\keyword{density estimation}
\keyword{measurement error}

