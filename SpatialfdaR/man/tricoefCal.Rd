\name{tricoefCal}
\alias{tricoefCal}
\title{
  Compute the coefficient matrix required to test of a point
  is inside a triangle.
}
\usage{
  tricoefCal(pts, tri)
}
\description{
Compute the coefficient matrix \code{tricoef} required to test
if a point is inside a triangle.
}
\arguments{
  \item{pts}{
    A two-column matrix of the locations of the vertices of the mesh.
  }
  \item{tri}{
    A three-column matrix of the indices in \code{pts} of the triangle vertices.
  }
}
\value{
  A matrix \code{tricoef}.
}
\seealso{
  \code{\link{insideIndex}}
}
\keyword{tricoefCal}
