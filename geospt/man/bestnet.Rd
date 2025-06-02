\name{bestnet}
\alias{bestnet}

\title{
Generate a SpatialPoints object corresponding to the best result obtained in an optimized network 
}
\description{
Generate a SpatialPoints object with the \emph{x} and \emph{y} coordinates corresponding to the best result obtained in an optimized network. The parameter to be passed to this function must be the result of \code{\link{seqPtsOptNet}} or \code{\link{simPtsOptNet}}
}
\usage{
bestnet(optimnet)
}

\arguments{
  \item{optimnet}{
object of class rbga resulting from \code{\link{seqPtsOptNet}} or \code{\link{simPtsOptNet}}
}
}
\value{
a SpatialPoints object
}
\seealso{
See function \code{\link[genalg]{rbga}} in the \code{genalg} package; for examples see \code{\link{seqPtsOptNet}} and \code{\link{simPtsOptNet}}
}

\keyword{ spatial }
