\name{combine.forecastCov}
\alias{combine.forecastCov}
\alias{combine.forecastCovEstimatorsWRTdata}
\alias{combine.forecastCovEstimatorsWRTtrue} 

\title{Combine 2 Forecast Cov Objects}
\description{Combine 2 forecastCov type objects.}
\usage{
    \method{combine}{forecastCov}(e1, e2)
    \method{combine}{forecastCovEstimatorsWRTdata}(e1, e2)
    \method{combine}{forecastCovEstimatorsWRTtrue}(e1, e2)
}
\arguments{
    \item{e1, e2}{
    Objects as returned by functions which calculate forecast covariances.}
}
\value{
An object as returned by functions which calculate forecast covariances.
}
\details{
Functions which calculate forecast covariances return lists. Usually multiple 
estimation techniques or models will be combined together when the object is
first formed. However, it is sometimes useful to add results calculated later
without re-doing the initial object.
}
\seealso{
\code{\link{combine}},
\code{\link{forecastCovEstimatorsWRTdata}},
\code{\link{forecastCovEstimatorsWRTtrue}}
\code{\link{forecastCov}}
}
\examples{#z <-  combine(obj1, obj2)}
\concept{DSE}
\keyword{ts}

