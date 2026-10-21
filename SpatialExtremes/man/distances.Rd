\name{distance}
\alias{distance}
\title{Computes distance between pairs of locations}
\description{This function computes euclidean distance or distance
  vector for each pair of  a set of spatial locations.
}
\usage{
distance(coord, vec = FALSE)
}
\arguments{
  \item{coord}{A matrix representing the coordinates of each
    locations. Each row corresponds to one location.}
  \item{vec}{Logical. If \code{FALSE} (default), the euclidean distance
    is computed. Otherwise, ``distance vectors'' are returned.}
}
\value{
  If \code{vec = FALSE}, this function returns a vector that gives the
  euclidean distance for each pair of locations. Otherwise, this is a
  matrix where each column correspond to one dimension -
  i.e. longitude, latitude, \dots
}
\seealso{
  \code{\link{dist}}
}
\examples{
coords <- cbind(seq(0, 10, by = 2), seq(10, 0, by = -2))
distance(coords)
distance(coords, vec = TRUE)
}
\author{Mathieu Ribatet}
\keyword{utilities}