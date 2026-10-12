\name{water}
\alias{water}
\docType{data}
\title{
Water quality (Ammonia) data
}
\description{
This water dataset records the amount of
dissolved ammonia at Susquehanna River Basin in the United States.
The dissolved ammonia data were observed biweekly in Susquehanna River at Towanda, PA, from 1988 to 2014, consisting of 524 data points, with detection limit at 0.02 (mg/l).
}
\usage{data(water)}
\format{
  A data frame with 524 observations on the following 4 variables.
  \describe{
    \item{\code{SDate}}{date of measuring}
    \item{\code{TNH3}}{response variable, the amount of dissolved ammonia}
    \item{\code{Delta}}{a logical vector indicating censored as 0 and uncensored as 1}
    \item{\code{Indep}}{a logical vector indicating the start of a new time series that is deemed to be independent from the previous one. For the water quality data, most measurements were taken biweekly but a few have longer time gaps from the previous measurements. In our analysis of the water quality data, we treat the date that is apart from the previous measurement date more than 14 days as the start of a new independent time series.}
  }
}
\source{
\url{https://www.srbc.net/portals/water-quality-projects/sediment-nutrient-assessment/}
}
\references{
Li, F., Tang, Y. and Wang, H. (2018) Copula-based Semiparametric Analysis for Time Series Data with Detection Limits, technical report.
}
\examples{
data(water)
str(water)
head(water)
}
\keyword{datasets}
