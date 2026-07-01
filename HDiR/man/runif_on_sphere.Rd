\name{runif_on_sphere}
\alias{runif_on_sphere}
 \title{
Random generation of a sample on a d-dimensional sphere of radius r}
\description{
This function generates a random sample on the unit sphere.}
\usage{
runif_on_sphere(n, d, r = 1)
}
 \arguments{
  \item{n}{
Size of the sample to be generated.}
  \item{d}{
Dimension of the space which contains the sphere.}
  \item{r}{
Radius of the sphere.}
}

\value{
A matrix of \code{n} rows and \code{d} columns containing the sample points.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\keyword{internal}
