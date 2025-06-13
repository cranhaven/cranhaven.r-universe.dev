\name{pdmod-package}
\alias{pdmod-package}
\alias{pdmod}
\docType{package}
\title{
Proximal/Distal Modeling Framework
}
\description{
In this model, Pavlovian phenomena conditioning phenomena (acquisition, extinction, spontaneous recovery and the partial reinforcement extinction effect) emerge from reward predictions of parallel neural circuits that combine according to their time-varying uncertainties. This package provides methods to compute the model for different parameter values and fit parameters to experimental data.
}
\details{
\tabular{ll}{
Package: \tab pdmod\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-27\cr
License: \tab GPL (>=2)\cr
}
For a given set of rewards/non-rewards paired with a signal in a Pavlovian conditioning experiment (specified as a \code{\link{TimedVector}}), the animal's response for a given set of parameter values can be computed with \code{\link{computeModel}}. Additionally, if experimental response data is available, the parameter values can be fit to the data using \code{\link{fitModel}}. Additional methods \code{\link{averageBySession}} and \code{\link{plot.pdmod}} are available to manipulate and plot model results.

\code{\link{TimedVector}} is a class used to associate reward/no-reward with a time schedule with helper methods \code{\link{c}}, \code{\link{isTimedVector}}, \code{\link{print}}, \code{\link{time}}, and 
\code{\link{verifyTimedVector}}.
}
\author{
Chloe Bracis

Maintainer: Chloe Bracis <cbracis@uw.edu>
}

\keyword{ package, Pavlovian conditioning, model, uncertainty }

