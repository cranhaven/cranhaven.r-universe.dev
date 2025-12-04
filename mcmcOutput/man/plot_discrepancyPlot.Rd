\name{discrepancyPlot}
\alias{discrepancyPlot}
\title{
Graphic comparison of observed vs simulated discrepancies
}
\description{
One way to assess the fit of a model is to calculate the discrepancy between the observed data and the values predicted by the model. For binomial and count data, the discrepancy will not be zero because the data are integers while the predictions are continuous. To assess whether the observed discrepancy is acceptable, we simulate new data according to the model and calculate discrepancies for the simulated data.

Function \code{discrepancyPlot} produces a scatter plot of the MCMC chains for observed vs simulated discrepancies and calculates and displays a p-value, the proportion of simulated discrepancy values that exceed the observed discrepancy.
}
\usage{
discrepancyPlot(object, observed, simulated, ...)
}
\arguments{
  \item{object}{
An object of class \code{mcmcOutput}, or an object which can be coerced to \code{mcmcOutput}, with MCMC chains for observed and simulated discrepancies.
}
  \item{observed}{
character; the name of the parameter for the observed discrepancy.
}
  \item{simulated}{
character; the name of the parameter for the simulated discrepancies.
}
  \item{\dots}{
additional graphical parameters passed to the \code{\link{plot.default}}.
}
}
\value{
Returns the proportion of simulated discrepancy values that exceed the observed discrepancy, often referred to as a "Bayesian p-value".
}
\author{
Mike Meredith.
}

\examples{
# Get some data
data(mcmcListExample)
( mco <- mcmcOutput(mcmcListExample) )
# Tobs and Tsim are the Freeman-Tukey discrepancy measures

discrepancyPlot(mco, observed="Tobs", simulated="Tsim") # defaults
discrepancyPlot(mco, observed="Tobs", simulated="Tsim",
    main="Salamanders", col='red')
}
\keyword{hplot}