\name{cqwMoRivOmaha}
\docType{data}
\alias{cqwMoRivOmaha}
\title{Continuously monitored (daily) data for 06610000 Missouri River at Omaha, Neb.}
\description{
  Continuously monitored (daily) streamflow and sediment data for U.S. Geological 
  Survey streamgage 06610000 Missouri River at Omaha, Neb., and streamflow and 
  sediment anomalies.
}
\details{
  The streamflow and sediment anomalies were generated using the R package 
  waterData (Ryberg and Vecchia, 2012).
}
\usage{cqwMoRivOmaha}
\keyword{datasets}
\format{A data frame containing 2,922 daily observations of two hydrologic variables,
  streamflow and sediment, and streamflow and sediment anomalies.
        There are  eight variables.
        \tabular{lll}{
        staid \tab character \tab USGS Station identification number \cr
        dates \tab date \tab date water-quality sample collected \cr
        dflow \tab numeric \tab daily mean streamflow, cubic feet per second \cr
        flowa30 \tab numeric \tab 30-day streamflow anomaly \cr
        flowa1 \tab numeric \tab 1-day streamflow anomaly \cr
        dsed \tab numeric \tab daily mean sediment concentration, milligrams per liter \cr
        seda30 \tab numeric \tab 30-day sediment anomaly \cr
        seda1 \tab numeric \tab 1-day sediment anomaly
        }
}
\examples{
  data(swData)
  
  # summary of water-quality concentrations
  apply(cqwMoRivOmaha[,3:8], 2, summary)
}
\source{Data provided by Patrick Phillips, U.S. Geological Survey,
        New York Water Science Center.}
\note{
  See Ryberg and Vecchia (2012) for more information on calculating the anomalies and for
  additional references documenting the use of streamflow anomalies in water-quality trend
  analysis studies. 
}
\references{
  Ryberg, K.R., and Vecchia, A.V., 2012, waterData---An R package for 
  retrieval, analysis, and anomaly calculation of daily hydrologic time 
  series data, version 1.0.0: U.S. Geological Survey Open-File Report 
  2012--1168, 8 p. [(]Also available at 
  \url{http://pubs.usgs.gov/of/2012/1168/}.]
}
  \keyword{datasets}