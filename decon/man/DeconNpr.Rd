% This is a template R document file
% Distributed under GPL 3 or later

\name{DeconNpr}
\alias{DeconNpr}
\title{Perform nonparametric regression with errors-in-variables}
\description{
  To perform nonparametric regression with errors-in-variables. The measurement errors can be either homoscedastic or heteroscedastic.
}
\usage{
DeconNpr(y,sig,z,x,error='normal',bw='dboot1',adjust=1,
	n=512,from,to,cut=3,na.rm=FALSE,grid=100,ub=2,...)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviations \eqn{\sigma}.  If homoscedastic errors, \eqn{sig} is a single value.
  If heteroscedastic errors,  \eqn{sig} is a vector of standard deviations having the same length 
  as \eqn{y}.}
  \item{z}{z is the dependent variable. }
  \item{x}{x is user-defined grids where the regression function will be evaluated. FFT method is not applicable if x is given. }
  \item{error}{Error distribution types: (1) 'normal' for normal errors; (2) 'laplacian' for Laplacian errors; (3) 'snormal' for a special case of small normal errors.}
  \item{bw}{Specifies the bandwidth. It can be a single numeric value which has been pre-determined; or computed with the specific bandwidth selector: 'dnrd' to compute the rule-of-thumb plugin bandwidth as suggested by Fan (1991); 'dmise' to compute the plugin bandwidth by minimizing MISE; 'dboot1' to compute the bootstrap bandwidth selector without resampling (Delaigle and Gijbels, 2004a), which minimizing the MISE bootstrap bandwidth selectors; 'boot2' to compute the smoothed bootstrap bandwidth selector with resampling.}
  \item{adjust}{adjust the range there the regression function is to be evaluated.  By default, \eqn{adjust=1}.}
  \item{n}{number of points where the regression function is to be evaluated.}
  \item{from}{the starting point where the regression functionF is to be evaluated.}
  \item{to}{the starting point where the regression function is to be evaluated.}
  \item{cut}{used to adjust the starting end ending points where the regression function is to be evaluated.}
  \item{na.rm}{ is set to FALSE by default: no NA value is allowed.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value is
 "ub=2".}
  \item{...}{control }
}
\details{
    FFT is currently not supported for nonparametric regression. 
}
\value{
An object of class ``Decon''.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Fan, J. and Truong, Y.K. (1993). Nonparametric regression with errors in variables. \emph{Annals of
Statistics}, 21(4), 1900-1925.

Delaigle, A. and Meister, A. (2007). Nonparametric regression estimation in the heteroscedastic errors-in-variables problem. \emph{Journal of the American Statistical Association}, 102, 1416-1426.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{DeconPdf},\link{DeconCdf}}.
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
# estimate the unknown density with measurement error
(m1 <-  DeconNpr(w, sig, y ,error="normal", from=0.9*min(x), to=0.9*max(x)))
# plot the results
plot(m1, col="red", lwd=3, lty=2, xlab="x", ylab="m(x)", main="", 
			zero.line=FALSE)
lines(ksmooth(x,y, kernel = "normal", 2, range.x=c(0.9*min(x),0.9*max(x))), 
			lwd=3, lty=1)
lines(ksmooth(w,y, kernel = "normal", 2, range.x=c(0.9*min(x),0.9*max(x))), 
			col="blue", lwd=3, lty=3)

}
\keyword{nonparametric}
\keyword{smooth}
\keyword{regression}
\keyword{errors-in-variables}
