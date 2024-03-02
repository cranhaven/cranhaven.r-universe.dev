\name{totalForecastCov}
\alias{totalForecastCov}
\title{Sum covariance of forecasts across all series}
\description{Sum covariance of forecasts across all series.}
\usage{
    totalForecastCov(obj, select=NULL)
}
\arguments{
    \item{obj}{An object  as returned by forecastCov.}
    \item{select}{
    Series to be select for summation. With the default all series are selected.}
}
\value{
An object similar to that returned by forecastCov, with the covariance
summed over all selected series.
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model1 <- estVARXar(eg1.DSE.data.diff)
model2 <- estVARXls(eg1.DSE.data.diff)
z <-  totalForecastCov(forecastCov(model1, model2,
                         data=trimNA(eg1.DSE.data.diff)))
}
\concept{DSE}
\keyword{ts}

