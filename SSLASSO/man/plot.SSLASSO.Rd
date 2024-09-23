\name{plot.SSLASSO}
\alias{plot.SSLASSO}
\title{Plot coefficients from a "SSLASSO" object}
\description{Produces a plot of the coefficient paths for a fitted
  \code{"SSLASSO"} object.}
\usage{
\method{plot}{SSLASSO}(x, ...)
}
\arguments{
  \item{x}{Fitted \code{"SSLASSO"} model.}
  \item{...}{Other graphical parameters to plot.}
}
\references{Rockova, V. and George, E.I. (2018) The Spike-and-Slab LASSO. Journal of the American Statistical Association.}

\author{Veronika Rockova <Veronika.Rockova@chicagobooth.edu>}
\seealso{\code{\link{SSLASSO}}}
\examples{
	
## Linear regression, where p>n
library(SSLASSO)

n=100
p=1000
X=matrix(rnorm(n*p), n, p)
beta=c(1,2,3,rep(0,p-3))
Y=X[,1]*beta[1]+X[,2]*beta[2]+X[,3]*beta[3]+rnorm(n)
lambda1<-0.1
lambda0<-seq(lambda1,100,length=50)
theta<-0.5


# Separable penalty with fixed theta

result<-SSLASSO(X, Y,penalty="separable", variance = "fixed", 
lambda1 = lambda1, lambda0 = lambda0,theta=theta)

plot(result)

}
\keyword{models}
\keyword{regression}
