\name{simpleRhat}
\alias{simpleRhat}

\title{
The Brooks-Gelman-Rubin (BGR) convergence diagnostic (deprecated)
}
\description{
This is now a wrapper for  \code{\link[mcmcOutput]{getRhat}}
}

\usage{
simpleRhat(object, n.chains, burnin=0)
}

\arguments{
  \item{object}{
an object of any class with MCMC output that can be coerced to class mcmcOutput.
}
  \item{n.chains}{
ignored
}
  \item{burnin}{
ignored
}
}

\value{
A named vector with the Rhat values.
}

\references{
Brooks, S.P. & Gelman, A. (1998) General methods for monitoring convergence of iterative simulations. \emph{Journal of Computational and Graphical Statistics}, 7, 434-455.

Spiegelhalter, Thomas, Best & Lunn (2003) WinBUGS User Manual Version 1.4, on line \href{https://www.mrc-bsu.cam.ac.uk/wp-content/uploads/manual14.pdf}{here}.
}

\author{
Mike Meredith
}


\examples{
# Get some output to use
data(salamanders)
y <- rowSums(salamanders)
( out <- BoccSS0(y, 5) )

simpleRhat(out)
}
