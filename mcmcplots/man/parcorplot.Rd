\name{parcorplot}
\alias{parcorplot}

\title{Correlation Plot for MCMC Draws of Model Parameters}

\description{
  Creates an image plot of posterior correlations between model   parameters from an MCMC simulation.
}

\usage{
parcorplot(mcmcout, parms = NULL, regex = NULL, random = NULL, col = gray(11:0/11), \dots)
}

\arguments{
  \item{mcmcout}{ posterior draws. This argument will be coerced to an \code{mcmc} object.}
  \item{parms}{ character vector specifying which subsets of parameters to plot.  If \code{NULL} and \code{regex=NULL}, \code{mcmcplot} will plot all parameters. Regular expressions are used to strip all numbers and punctuation out of the parameter names to find the parameters that match the character strings in \code{parms}.}
  \item{regex}{ character vector of regular expressions denoting groups of parameters to plot.}
  \item{random}{ integer specifying how many parameters from each group will be randomly selected for plotting.  This argument is useful when \code{mcmcout} has a large number of parameters (e.g., from a hierarchical model).  If \code{NULL}, \code{mcmcplot} will plot all parameters.}
  \item{col}{ colors to be used in the plot.}
  \item{\dots}{ further arguments that are passed to the \code{corplot} function.}
}

\details{
  The \code{parcorplot} is a wrapper function to \code{corplot} that allows the use of arguments \code{parms}, \code{regex}, and \code{random} to conveniently select parameters from an MCMC simulation to plot with \code{corplot}.
}

\value{
  Creates a plot.
}

\author{S. McKay Curtis}

\seealso{
  \code{\link{corplot}}, \code{\link{parms2plot}}
}

\examples{
Rho1 <- outer(1:10, 1:10, function(i, j) 0.5^(abs(i-j)))
Rho2 <- outer(1:5, 1:5, function(i, j) 0.25^(i!=j))
dat1 <- t(apply(matrix(rnorm(10*1000), 1000, 10), 1, function(z, Rho1) t(Rho1)\%*\%z, Rho1))
dat2 <- t(apply(matrix(rnorm(5*1000), 1000, 5), 1, function(z, Rho2) t(Rho2)\%*\%z, Rho2))
colnames(dat1) <- paste("theta[", 1:10, "]", sep="")
colnames(dat2) <- paste("alpha[", 1:5, "]", sep="")
dat <- cbind(dat1, dat2)
parcorplot(dat, "theta", col=gray(31:0/31), cex.axis=0.75)
parcorplot(dat, col=heat.colors(31), cex.axis=0.75)
parcorplot(dat, col=topo.colors(31), cex.axis=0.75)
parcorplot(dat, col=terrain.colors(31), cex.axis=0.75)
parcorplot(dat, col=cm.colors(31), cex.axis=0.75)
}

\keyword{ hplot }
