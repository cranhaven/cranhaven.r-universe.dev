\name{plotComb}
\alias{plotComb}
\title{
Display a posterior probability distribution from the comb method
}
\description{
Plot the posterior probability distribution for a single parameter calculated using the comb method described by Kruschke (2015).
}
\usage{
plotComb(x, y, credMass = 0.95, plot = TRUE, showMode = FALSE, shadeHDI = NULL, ...)
}
\arguments{
  \item{x}{
A vector of equally-spaced possible values for the parameter. The range should cover all values of the parameter with non-negligible probability. (To restrict the range displayed in the plot, use \code{xlim}.)
}
  \item{y}{
A vector of probabilities corresponding to the values in \code{x}.
}
  \item{credMass}{
the probability mass to include in credible intervals; set to NULL to suppress plotting of credible intervals.
}
  \item{plot}{
logical: if TRUE, the posterior is plotted.
}
  \item{showMode}{
logical: if TRUE, the mode is displayed instead of the mean.
}
  \item{shadeHDI}{
specifies a colour to shade the area under the curve corresponding to the HDI; NULL for no shading. Use\code{colours()} to see a list of possible colours.
}
  \item{\dots}{
additional graphical parameters.
}
}
\details{
The function calculates the Highest Density Interval (HDI). A multi-modal distribution may have a disjoint HDI, in which case the ends of each segment are calculated. No interpolation is done, and the end points correspond to values of the parameter in \code{x}; precision will be determined by the resolution of \code{x}.

If \code{plot = TRUE}, the probability density is plotted together with either the mean or the mode and the HDI.

\figure{plotPost2.jpg}

}
\value{
Returns a matrix with the upper and lower limits of the HDI. If the HDI is disjoint, this matrix will have more than 1 row. It has attributes \code{credMass} and \code{height}, giving the height of the probability curve corresponding to the ends of the HDI.
}
\author{
Mike Meredith
}

\seealso{
For details of the HDI calculation, see \code{\link{hdi}}.
}
\examples{
# Generate some data:
N <- 0:100
post <- dpois(N, 25)
# Do the plots:
plotComb(N, post)
plotComb(N, post, showMode=TRUE, shadeHDI='pink', xlim=c(10, 50))

# A bimodal distribution:
post2 <- (dnorm(N, 28, 8) + dnorm(N, 70, 11)) / 2
plotComb(N, post2, credMass=0.99, shade='pink')
plotComb(N, post2, credMass=0.80, shade='grey')

}
\keyword{hplot}