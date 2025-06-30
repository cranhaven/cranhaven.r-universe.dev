\name{examplecavdat}
\docType{data}
\alias{examplecavdat}
\title{Example continuous ancillary variable data.}
\description{
  This is an example of the continuous ancillary data that is passed 
  internally to subfunctions of \code{\link{fitswavecav}}.  It is provided
  here for use with examples of internal functions.
}
\usage{examplecavdat}
\keyword{datasets}
\format{A data frame containing 2,893 data variables and 30-day and 
  1-day streamflow anomalies (Ryberg and Vecchia, 2012).
  \tabular{lll}{
  yrx \tab numeric \tab Year \cr
  mox \tab numeric \tab Month \cr
  dax \tab numeric \tab Day \cr
  jdayx \tab numeric \tab Julian day from first day water year for start year in \code{\link{fitswavecav}} \cr
  flowa30 \tab numeric \tab 30-day streamflow anomaly \cr
  flowa1 \tab numeric \tab 1-day streamflow anomaly \cr
  }
}
\examples{
data(swData)
head(examplecavdat)
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}
\references{
  Ryberg, K.R., and Vecchia, A.V., 2012, waterData--An R package for retrieval,
  analysis, and anomaly calculation of daily hydrologic time series data, 
  version 1.0: U.S. Geological Survey Open-File Report 2012--1168, 8 p.
  [Also available at \url{http://pubs.usgs.gov/of/2012/1168/}.]
}
\keyword{datasets}


