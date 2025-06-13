\name{OVL}
\alias{OVL}
\title{
  Overlap quantification for LPS object
}

\description{
  Quantify the overlap between gaussian distributions of the two group scores, to assess model efficiency (best models should not overlap, to prevent from false discovery).
}

\usage{
  OVL(means, sds, cutoff=1e-4, n=1e4)
}

\arguments{
  \item{means}{
    Numeric vector of two values, the means of the gaussian distributions.
}
  \item{sds}{
    Numeric vector of two values, the standard deviations of the gaussian distributions.
}
  \item{cutoff}{
    Single numeric value, minimal quantile for integration range definition (distributions will be considered between their \code{cutoff} and \code{1 - cutoff} quantiles only). The lesser it is, the more precise the returned value will be.
}
  \item{n}{
    Single integer value, the amount of equi-distant points to use for the computation. The greater it is, the more precise the returned value will be.
}
}

\value{
  Returns the proportion of the overlap between the two gaussian distributions N1 and N2, i.e. \code{min(N1, N2) / (N1 + N2)}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{LPS-class}}, \code{\link{LPS}}, \code{link{dnorm}}
}

\examples{
  # Full overlap between identical distributions
  OVL(c(0,0), c(1,1))
  
  # Increasing shift
  OVL(c(0,1), c(1,1))
  OVL(c(0,2), c(1,1))
  OVL(c(0,3), c(1,1))
  OVL(c(0,10), c(1,1))
}

