\name{postPriorOverlap}
\alias{postPriorOverlap}
\title{
Overlap between posterior and prior probability distributions.
}
\description{
Calculates and displays the overlap between a posterior distribution (as a vector of values, typically draws from an MCMC process) and a prior distribution (as a vector of values or as a function).
}
\usage{
postPriorOverlap(x, prior, priorPars, breaks=NULL,
           hcols=c("skyblue", "yellow", "green", "white"), ...)
}
\arguments{
  \item{x}{
a vector of values drawn from the target distribution.
}
  \item{prior}{
\emph{either} a vector of values drawn from the prior distribution \emph{or} the name for the density function of the distribution; standard R functions for this have a \code{d-} prefix, eg. \code{dbeta}.
}
  \item{priorPars}{
a named list of parameters to be passed to \code{prior} when it is a function ; see the examples. Ignored if \code{prior} is a numeric vector.
}
  \item{breaks}{
controls the histogram break points or the number of bars; see \code{\link{hist}}.
}
  \item{hcols}{
a vector of four colours for the histograms: posterior, prior, overlap, and borders. See the Color Specification section of \code{\link{par}}
}
  \item{\dots}{
other graphical parameters.
}
}
\value{
Returns the overlap, the area lying under the lower of the two density curves.
}
\author{
Mike Meredith
}

\examples{
# Generate some data
foo <- rbeta(1e4, 5, 7)

# check overlap with a Beta(0.2, 0.2) prior:
postPriorOverlap(foo, dbeta, list(shape1=0.2, shape2=0.2))

# check overlap with a Uniform(0, 1) prior:
postPriorOverlap(foo, runif(1e6))
}
