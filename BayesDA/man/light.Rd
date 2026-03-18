\name{light}
\alias{light}
\docType{data}
\title{ Simon Newcomb's Measurements for the Speed of Light   }
\description{
    Simon Newcomb's measurements (1882) to measure the speed of light. 
    The data are recordes as deviations from 24800 nanoseconds.
}
\usage{data(light)}
\format{
  The format is:
 atomic [1:66] 28 26 33 24 34 -44 27 16 40 -2 ...
 - attr(*, "comment")= chr "Units: deviations from 24800 nanoseconds"
}
\details{
  The currently accepted value for the speed of light on this scale 
  is 33.0.
}
\examples{
data(light)
comment(light)
hist(light, breaks=40)
abline(v=33.0, col="red")
}
\keyword{datasets}
\concept{robust}

