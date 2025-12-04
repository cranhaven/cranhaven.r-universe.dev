\name{richCurve}
\alias{richCurve}
\alias{richSobs}
\alias{richSingle}
\alias{richDouble}
\alias{richUnique}
\alias{richDuplicate}

\title{
Species richness estimates based on accumulation curves
}
\description{
Provides a shell into which species richness estimators may be plugged to provide estimates based on species accumulation curves, as provided by EstimateS.
}
\usage{
richCurve(obsMat, FUNC, runs = 10, ...)

richSobs(incVec)
richSingle(cntVec)
richDouble(cntVec)
richUnique(incMat)
richDuplicate(incMat)
}
\arguments{
  \item{incVec}{
a vector of species incidences (presences) in one or more samples; a vector of counts or a species x sites matrix of incidences or counts may be supplied.
}
  \item{cntVec}{
a vector of species counts (abundances); a species x sites matrix of counts may be supplied and will be converted to a vector with \code{rowSums}.
}
  \item{incMat}{
a 1/0 matrix of species incidence (presence), species x sites. A matrix of counts may also be provided.
}
  \item{obsMat}{
a matrix of species counts, species x sites; a matrix of incidences will be sufficient if accepted by FUNC.
}
  \item{FUNC}{
a function to estimate species richness based on a matrix of observations; see \code{\link{Species richness estimators}} for examples.
}
  \item{runs}{
the number of randomisations of samples (ie. columns in the input matrix) to perform to calculate mean and standard deviation.
}
  \item{\dots}{
additional arguments passed to FUNC.
}
}
\details{
The reliability of estimates of species richness depends on the sampling effort. To investigate this effect, and judge whether the current sampling effort is adequate, we calculate richness estimates for subsets of the data. Assuming that the columns of the data matrix are independent samples from the population, \code{richCurve} calculates estimates for 1 sample, 2 samples, and so on. This is repeated for many runs, and the mean and standard deviation calculated.

The other functions documented here are trivial, but useful for plugging into \code{richCurve}:

  \code{richSobs } : the number of species observed.

  \code{richSingle }: the number of singletons, ie. species represented by just 1 individual in the pooled samples.

  \code{richDouble } : the number of doubletons, ie. species represented by exactly 2 individuals in the pooled samples.

  \code{richUnique } : the number of uniques, ie. species represented in just one sample.

  \code{richDuplicate } : the number of duplicates, ie. species represented in exactly 2 samples.
}
\value{
\code{richCurve} returns a list with elements:
  \item{mean }{A matrix (possibly 1-column) with a row for each sample and a column for each value returned by FUNC.}
  \item{SD }{The corresponding matrix with the standard deviations of the estimates from the runs.}

The other functions return scalars.
}
\author{
Mike Meredith
}
\examples{
data(seedbank)
plot(richCurve(seedbank, richSobs)$mean, type='l', ylim=c(0, 35))
lines(richCurve(seedbank, richSingle)$mean, col='blue')
lines(richCurve(seedbank, richDouble)$mean, col='blue', lty=2)
}
