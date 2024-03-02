\name{TSdata.forecastCov}
\alias{TSdata.forecastCov}
\alias{TSmodel.forecastCov}

\title{TS Extractor Specific Methods}
\description{See the generic function description.}
\usage{
    \method{TSdata}{forecastCov}(data, ...)
    \method{TSmodel}{forecastCov}(obj, select=1, ...)
}
\arguments{
    \item{data}{an object from which to extract the TSdata.}
    \item{obj}{an object from which to extract the TSmodel or TSestModel.}
    \item{select}{an integer indicating which of multiple models to extract.}
    \item{...}{arguments to be passed to other methods.}
}

\seealso{ 
    \code{\link{TSdata}} 
    \code{\link{TSestModel}} 
    \code{\link{TSmodel}} 
}

\concept{DSE}
\keyword{ts}

