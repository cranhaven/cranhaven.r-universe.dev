\name{data.dti}
\alias{data.dti}
\docType{data}
\title{
DTI data.
}
\description{
A diffusion tensor imaging study comparing brain activity of six dyslexic children versus six normal controls. Two-sample tests produced z-values at \eqn{N = 15443} voxels (3-dimensional brain locations), with each \eqn{z_i \sim N(0,1)} under the null hypothesis of no difference between the dyslexic and normal children.}
\usage{data(data.dti)}
\format{
  A data frame with 15443 observations on the following 4 variables.
  \describe{
    \item{\code{coordx}}{A list of x coordinates}
    \item{\code{coordy}}{A list of y coordinates}
    \item{\code{coordz}}{A list of z coordinates}
    \item{\code{z}}{The \eqn{z}-values.}
  }
}
\source{
http://statweb.stanford.edu/~ckirby/brad/LSI/datasets-and-programs/datasets.html
}
\references{
Efron, B. (2012). "Large-scale inference: empirical Bayes methods for estimation, testing, and prediction". Cambridge University Press.
}
\keyword{datasets}
