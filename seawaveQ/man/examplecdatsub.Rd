\name{examplecdatsub}
\docType{data}
\alias{examplecdatsub}
\title{Example water-quality data.}
\description{
  This is an example of the water-quality data that is passed 
  internally to subfunctions of \code{\link{fitswavecav}}.  It is provided
  here for use with examples of internal functions.
}
\usage{examplecdatsub}
\keyword{datasets}
\format{
  A data frame containing 115 observations of 10 variables.  The date variables
  were internally calculated.  The columns R04041 and P04041 are a subset of 
  \code{\link{qwMoRivOmaha}} and the 30-day and 1-day streamflow and sediment anomalies 
  are a subset of \code{\link{cqwMoRivOmaha}}.
  \tabular{lll}{
  yrc \tab numeric \tab Year \cr
  moc \tab numeric \tab Month \cr
  dac \tab numeric \tab Day \cr
  jdayc \tab numeric \tab Julian day from first day of start year in \code{\link{fitswavecav}} \cr
  flowa30 \tab numeric \tab 30-day streamflow anomaly \cr
  flowa1 \tab numeric \tab 1-day streamflow anomaly \cr
  seda30 \tab numeric \tab 30-day sediment anomaly \cr
  seda1 \tab numeric \tab 1-day sediment anomaly \cr
  }
}
\seealso{
  \code{\link{qwMoRivOmaha}}
  \code{\link{cqwMoRivOmaha}}
}
\examples{
  data(swData)
  head(examplecdatsub)
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}


