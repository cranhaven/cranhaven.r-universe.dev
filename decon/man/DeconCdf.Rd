% This is a template R document file
% Distributed under GPL 3 or later

\name{DeconCdf}
\alias{DeconCdf}
\title{Estimating cumulative distribution function from data with measurement error}
\description{
  To compute the cumulative distribution function from data coupled with measurement error. The measurement errors can be either homoscedastic or heteroscedastic.
}
\usage{
DeconCdf(y,sig,x,error="normal",bw="dboot1",adjust=1,
	n=512,from,to,cut=3,na.rm=FALSE,grid=100,ub=2,...)
}
\arguments{
  \item{y}{The observed data.  It is a vector of length at least 3.}
  \item{sig}{The standard deviations \eqn{\sigma}.  If homoscedastic errors, \eqn{sig} is a single value.
  If heteroscedastic errors,  \eqn{sig} is a vector of standard deviations having the same length 
  as \eqn{y}.}
  \item{x}{x is user-defined grids where the CDF will be evaluated. FFT method is not applicable if x is given. }
  \item{error}{Error distribution types: (1) 'normal' for normal errors; (2) 'laplacian' for Laplacian errors; (3) 'snormal' for a special case of small normal errors.}
  \item{bw}{Specifies the bandwidth. It can be a single numeric value which has been pre-determined; or computed with the specific bandwidth selector: 'dnrd' to compute the rule-of-thumb plugin bandwidth as suggested by Fan (1991); 'dmise' to compute the plugin bandwidth by minimizing MISE; 'dboot1' to compute the bootstrap bandwidth selector without resampling (Delaigle and Gijbels, 2004a), which minimizing the MISE bootstrap bandwidth selectors; 'boot2' to compute the smoothed bootstrap bandwidth selector with resampling.}
  \item{adjust}{adjust the range there the CDF is to be evaluated.  By default, \eqn{adjust=1}.}
  \item{n}{number of points where the CDF is to be evaluated.}
  \item{from}{the starting point where the CDF is to be evaluated.}
  \item{to}{the starting point where the CDF is to be evaluated.}
  \item{cut}{used to adjust the starting end ending points where the CDF is to be evaluated.}
  \item{na.rm}{ is set to FALSE by default: no NA value is allowed.}
  \item{grid}{the grid number to search the optimal bandwidth when a bandwidth selector was specified in bw. Default value "grid=100".}
  \item{ub}{the upper boundary to search the optimal bandwidth, default value is
 "ub=2".}
  \item{...}{control }
}
\details{
    FFT is currently not supported for CDF computing. 
}
\value{
An object of class ``Decon''.
}
\author{
  X.F. Wang \email{wangx6@ccf.org}

  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{

Delaigle, A. and Gijbels, I. (2004). Bootstrap bandwidth selection in kernel density estimation from a contaminated sample. \emph{Annals of the Institute of Statistical Mathematics}, 56(1), 19-47.

Fan, J. (1991). On the optimal rates of convergence for nonparametric deconvolution problems. \emph{The Annals of Statistics}, 19, 1257-1272. 

Fan, J. (1992). Deconvolution with supersmooth distributions. \emph{The Canadian Journal of Statistics}, 20, 155-169.


Hall, P. and Lahiri, S.N. (2008). Estimation of distributions, moments and quantiles in deconvolution problems. \emph{Annals of Statistics}, 36(5), 2110-2134.

Stefanski L.A. and Carroll R.J. (1990). Deconvoluting kernel density estimators. \emph{Statistics}, 21, 169-184.

Wang, X.F., Fan, Z. and Wang, B. (2010). Estimating smooth distribution function in the presence of heterogeneous measurement errors. \emph{Computational Statistics and Data Analysis}, 54, 25-36.

Wang, X.F. and Wang, B. (2011). Deconvolution estimation in measurement error models: The R package decon. \emph{Journal of Statistical Software}, 39(10), 1-24.
}
\seealso{
 \code{\link{DeconPdf}}, \code{\link{DeconNpr}}.
}
\examples{

#####################
## the R function to estimate the smooth distribution function
#SDF <- function (x, bw = bw.nrd0(x), n = 512, lim=1){
#        dx <- lim*sd(x)/20 
#        xgrid <- seq(min(x)-dx, max(x)+dx, length = n)
#        Fhat <- sapply(x, function(x) pnorm((xgrid-x)/bw)) 
#        return(list(x = xgrid, y = rowMeans(Fhat)))
#    }

## Case study: homoscedastic normal errors
n2 <- 100
x2 <- c(rnorm(n2/2,-3,1),rnorm(n2/2,3,1))
sig2 <- .8
u2 <- rnorm(n2, sd=sig2)
w2 <- x2+u2
# estimate the bandwidth with the bootstrap method with resampling
bw2 <- bw.dboot2(w2,sig=sig2, error="normal")
# estimate the distribution function with measurement error
F2 <-  DeconCdf(w2,sig2,error='normal',bw=bw2)
plot(F2,  col="red", lwd=3, lty=2, xlab="x", ylab="F(x)", main="")

#lines(SDF(x2), lwd=3, lty=1)
#lines(SDF(w2), col="blue", lwd=3, lty=3)

}
\keyword{nonparametric}
\keyword{smooth}
\keyword{measurement error}
