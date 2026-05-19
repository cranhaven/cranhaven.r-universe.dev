\name{omp}
\alias{omp}
\title{OpenMP-Parallel DRESS Backend}
\description{
An environment providing OpenMP-parallel versions of DRESS functions.
Use \code{omp$fit(...)}, \code{omp$delta_fit(...)}, and \code{omp$nabla_fit(...)} for
OpenMP edge-parallel fitting, subgraph-parallel delta fitting, and
tuple-parallel nabla fitting.

Same parameters as \code{\link{fit}}, \code{\link{delta_fit}}, and \code{\link{nabla_fit}}.
}
\usage{
omp
}
\examples{
\dontrun{
  r <- omp$fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
  d <- omp$delta_fit(3L, c(0L,1L,0L), c(1L,2L,2L), k = 1L)
  n <- omp$nabla_fit(3L, c(0L,1L,0L), c(1L,2L,2L), k = 1L)
}
}
\keyword{graphs}
