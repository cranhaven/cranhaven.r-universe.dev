\name{getFDR}
\alias{getFDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute false discovery rates and related statistics
}
\description{
This function is especially useful in the context of genetic data.
}
\usage{
getFDR(dat, threshold = 0.7, idxNotDiff = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A mixture, i.e. a \code{CAMAN.object}.
}
  \item{threshold}{
numeric, should be between 0 and 1 to be meaningful.
}
  \item{idxNotDiff}{
integer, index of component for which computations are required.
}
}
\details{
See Schlattmann (2009) for more details.
}
\value{
A list with components \code{FDR}, \code{FNDR}, \code{FPR} and \code{FNR}.}
\references{
Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
}
\author{
Peter Schlattmann and Johannes Hoehne
}

\concept{false-discovery-rate}