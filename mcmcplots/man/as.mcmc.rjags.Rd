\name{as.mcmc.rjags}
\alias{as.mcmc.rjags}

\title{Convert an rjags Object to an mcmc or mcmc.list Object.}

\description{
  Converts an \code{rjags} object to an \code{mcmc} object.
}

\usage{
\method{as.mcmc}{rjags}(x, \dots)
}

\arguments{
  \item{x}{ \code{rjags} object }
  \item{\dots}{unused}
}

\details{
  If \code{x} contains multiple chains, the function returns an \code{mcmc.list} object.  Otherwise, the function returns an \code{mcmc} object.
}

\value{
  An \code{mcmc.list} or \code{mcmc} object.
}

\author{ S. McKay Curtis }

\seealso{
  \pkg{coda} package.
}

\examples{
## None ##
}

\keyword{manip}
