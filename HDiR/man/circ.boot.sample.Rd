\name{circ.boot.sample}
\alias{circ.boot.sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap samples from a circular kernel estimator
}
\description{
This function generates bootstrap samples from a circular kernel estimator.
}
\usage{
circ.boot.sample(sample, n, bw)
}
\arguments{
  \item{sample}{
Numeric vector containing the sample of angles in radians determining a set of points on the unit circle.
}
  \item{n}{
Sample size to be generated.}
  \item{bw}{
Bandwidth parameter.}
}

\value{
A numeric vector containing a bootstrap sample of angles in radians of size \code{n}.
}

\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\keyword{internal}

