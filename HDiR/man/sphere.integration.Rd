\name{sphere.integration}
\alias{sphere.integration}
 \title{
Numerical integration on the unit sphere}
\description{
This function calculates the integral of a function \eqn{f} on the unit sphere.}
\usage{
sphere.integration(f, mesh = 40, deg = 3)
}
 \arguments{
  \item{f}{
Function to be integrated.}
  \item{mesh}{
Triangular mesh to be used. More details can be found in the description of \code{sphere.hdr} function.
}
  \item{deg}{
Degree of the quadrature rules for triangles. More details can be found in the description of \code{sphere.hdr} function.}
}

\value{
A numeric value of the integral.
}

\author{
Andrés Prieto, Rosa M. Crujeiras and Paula Saavedra-Nieves.
}
\keyword{internal}
