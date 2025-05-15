\name{Regression.Integrator.Fit-methods}
\docType{methods}
\alias{Regression.Integrator.Fit}
\alias{Regression.Integrator.Fit-methods}
\alias{Regression.Select.Fit}
\alias{Regression.Select.Fit-methods}
\title{ Generic Integrator Methods in Package \pkg{EnsembleBase}}
\description{
Generic methods that can be extended and used in constructing integrator algorithms by other packages.
}

\usage{
Regression.Integrator.Fit(object, X, y, print.level=1)
Regression.Select.Fit(object, X, y, print.level=1)
}

\arguments{
  \item{object}{An object typically containing all configuration parameters of a particular integrator algorithm or operations.}
  \item{X}{Matrix of covariates.}
  \item{y}{Vector of response variables.}
  \item{print.level}{Verbosity level.}
}

\keyword{methods}

