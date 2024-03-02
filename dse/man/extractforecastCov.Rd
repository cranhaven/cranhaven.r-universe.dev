\name{extractforecastCov}
\alias{extractforecastCov}
\alias{extractforecastCov.forecastCovEstimatorsWRTdata}
\alias{extractforecastCov.forecastCovEstimatorsFromModel}

\title{Extract Forecast Covariance}
\description{extract forecastCov from objects}
\usage{
    extractforecastCov(e, n)
    \method{extractforecastCov}{forecastCovEstimatorsWRTdata}(e, n)
    \method{extractforecastCov}{forecastCovEstimatorsFromModel}(e, n)
}
\arguments{
    \item{e}{A "forecastCovEstimatorsWRTdata", "forecastCov" object.}
    \item{n}{A vector on integers.}
}
\value{A forecastCov object.}
\details{
    Select a subset of models and their forecast covariances from a
    larger object.
}
\seealso{
    \code{\link{forecastCov}}
}

\concept{DSE}
\keyword{ts}

