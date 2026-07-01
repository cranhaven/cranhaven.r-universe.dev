\name{sphere.boot.sample}
\alias{sphere.boot.sample}
 \title{
Bootstrap samples from a spherical kernel estimator}
\description{
This function generates bootstrap samples from a spherical kernel estimator.}
\usage{
sphere.boot.sample(sample, n, bw)
}
 \arguments{
  \item{sample}{
Matrix of \code{n} rows and three columns containing the sample of points on the unit sphere.}
  \item{n}{
Sample size to be generated.}
  \item{bw}{
Bandwidth parameter.}
}

\value{
A matrix containing a bootstrap sample on the unit sphere.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\keyword{internal}
