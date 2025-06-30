\name{examplecavmat}
\docType{data}
\alias{examplecavmat}
\title{Example continuous ancillary variable matrix.}
\description{
  This is an example of the continuous ancillary matrix that is passed 
  internally to subfunctions of \code{\link{fitswavecav}}.  It is provided
  here for use with examples of internal functions.
}
\usage{examplecavmat}
\keyword{datasets}
\format{A matrix containing 115 30-day and 1-day streamflow anomalies 
  (Ryberg and Vecchia, 2012).
  \tabular{lll}{
  flowa30 \tab numeric \tab 30-day streamflow anomaly \cr
  flowa1 \tab numeric \tab 1-day streamflow anomaly \cr
  }
}
\examples{
data(swData)
head(examplecavmat)
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}



