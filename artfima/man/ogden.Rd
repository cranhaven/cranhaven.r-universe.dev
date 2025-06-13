\name{ogden}
\alias{ogden}
\docType{data}
\title{
Mean Annual St.  Lawrence Riverflow
}
\description{
Mean Annual unregulated riverflows of the St.  Lawrence River at Ogdensburg, 
N.Y. from 1860 to 1957 is comprised of 97 consecutive observations.
}

\usage{data("ogden")}
\format{
  The format is:
 Time-Series [1:97] from 1860 to 1956: 7788 8040 7733 7528 7528 ...
}

\details{
Hipel and McLeod (1994, 2005) showed this time series could be adequately
modelled using an AR(1).
}

\source{
  Hipel, K.W. and McLeod, A.I., (1994, 2005).
  Time Series Modelling of Water Resources and Environmental Systems.
   Electronic reprint of our book orginally published in 1994.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/}.
}

\examples{
data(ogden)
#compare fits of AR(1) and TFD
arima(ogden, order=c(1,0,0))
artfima(ogden) #this model has one more parameter

#Find AIC/BIC 3 best models. Takes about 10 sec
\dontrun{
system.time(ans <- bestModels(ogden, nbest=3))
summary(ans) #summary provides plausibility as well as scores
}
}

\keyword{datasets}
