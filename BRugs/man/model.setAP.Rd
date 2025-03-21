\name{modelSetAP}
\alias{modelSetAP}
\alias{modelSetIts}
\alias{modelSetOR}
\title{Changing settings of updating algorithms}
\description{These functions change adaptive phase, iterations, and
  overRelaxation settings. Currently only supported on Windows, not Linux.}
\usage{
modelSetAP(factoryName, adaptivePhase)
modelSetIts(factoryName, iterations)
modelSetOR(factoryName, overRelaxation)
}
\arguments{
  \item{factoryName}{String defining which particular MCMC updating algorithm is to be tuned.
Technically this string is the type name of the factory object used to create the updater,
for example \sQuote{UpdaterMetnormal.Factory} for the random walk Metropolis sampler.}
  \item{adaptivePhase}{length of the updater's adaptive phase}
  \item{iterations}{number of times an iterative algorithm is run before a failure is reported}
  \item{overRelaxation}{amount of over relaxation the updater uses}
}
\details{
Once a model has been compiled, the various updating algorithms required in order to perform the MCMC
simulation may be \sQuote{tuned} somewhat via these three functions.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{update}
\concept{adaptive Phase}
\concept{iteration}
\concept{overRelaxation}
