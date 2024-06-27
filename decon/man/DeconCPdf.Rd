% This is a template R document file
% Distributed under GPL 3 or later

\name{DeconCPdf}
\alias{DeconCPdf}
\title{Estimating conditional probability density function from data with measurement error}
\description{
  To compute the conditional probability density function from data with measurement error. The measurement errors have to  be homoscedastic.
}
\usage{
DeconCPdf(y,sig,y0,error='normal',bw1='dboot1',bw2='nrd0',adjust=1,
	fft=FALSE,n=512,from,to,cut=3,na.rm=FALSE,grid=100,ub=2,tol=0,...)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviations \eqn{\sigma}.  If homoscedastic errors, \eqn{sig} is a single value.
  If heteroscedastic errors,  \eqn{sig} is a vector of standard deviations having the same length 
  as \eqn{y}.}
  \item{y0}{The given conditional data point in the conditional density f(x|y=y0).}
  \item{error}{Error distribution types: (1) 'normal' for normal errors; (2) 'laplacian' for Laplacian errors; (3) 'snormal' for a special case of small normal errors.}
  \item{bw1}{The bandwidth for the deconvolution density \eqn{f_X}. It can be a single numeric value which has been pre-determined; or computed with the specific bandwidth selector: 'dnrd' to compute the rule-of-thumb plugin bandwidth as suggested by Fan (1991); 'dmise' to compute the plugin bandwidth by minimizing MISE; 'dboot1' to compute the bootstrap bandwidth selector without resampling (Delaigle and Gijbels, 2004a), which minimizing the MISE bootstrap bandwidth selectors; 'boot2' to compute the smoothed bootstrap bandwidth selector with resampling.}
  \item{bw2}{The bandwidth for the kernel density \eqn{f_Y}. It can be a single numeric value which has been pre-determined; or computed with the specific bandwidth selector: 'nrd0','nrd','ucv', 'bcv', and 'SJ' (see the "density" function in R).}
  \item{adjust}{adjust the range there the PDF is to be evaluated.  By default, \eqn{adjust=1}.}
  \item{fft}{To specify the method to compute the PDF.  'fft=FALSE' to compute directly; 'fft=TRUE' to compute the PDF by using the Fast Fourier Transformation.}
  \item{n}{number of points where the conditional PDF is to be evaluated.}
  \item{from}{the starting point where the conditional PDF is to be evaluated.}
  \item{to}{the starting point where the conditional PDF is to be evaluated.}
  \item{cut}{used to adjust the starting end ending points where the conditional  PDF is to be evaluated.}
  \item{na.rm}{ is set to FALSE by default: no NA value is allowed.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value is
 "ub=2".}
  \item{tol}{the parameter to avoid the estimate of f(y|x) too small. The default vaule is 0. It can not exceed 0.05.}
  \item{...}{control }
}
\details{
  If the number of points to be evaluated is too small (less than 32), a direct computing method is preferred.  The current version can support up to \eqn{2^21} points where the conditional PDF to be computed. 
}
\value{
An object of class ``Decon''.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{
Fan, J. (1991). On the optimal rates of convergence for nonparametric deconvolution problems. \emph{The Annals of Statistics}, 19, 1257-1272. 

Wang XF, Ye D (2010). Conditional density estimation with measurement error. Technical Report.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{DeconPdf}}.
}
\examples{

n <- 100
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
sig <- .8
u <- rnorm(n,sd=sig)
w <- x+u

f1 <-  DeconCPdf(w,sig, y0=-2.5, error='normal')
#f2 <-  DeconCPdf(w,sig, y0=0, error='normal')
#f3 <-  DeconCPdf(w,sig, y0=2.5, error='normal')

#par(mfrow=c(2,2))
plot(density(w), main="f_w", xlab="w")
plot(f1, main="f1", xlab="x")
#plot(f2, main="f2", xlab="x")
#plot(f3, main="f3", xlab="x")

}
\keyword{nonparametric}
\keyword{smooth}
\keyword{measurement error}
