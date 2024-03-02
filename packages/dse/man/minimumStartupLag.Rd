\name{minimumStartupLag}
\alias{minimumStartupLag}
\alias{minimumStartupLag.SS}
\alias{minimumStartupLag.ARMA}
\alias{minimumStartupLag.TSestModel}
\alias{startShift}

\title{Starting Periods Required}
\description{Number of Starting Periods Required for a Model}
\usage{
    minimumStartupLag(model)
    \method{minimumStartupLag}{SS}(model)
    \method{minimumStartupLag}{ARMA}(model)
    \method{minimumStartupLag}{TSestModel}(model)
    startShift(model,data, y0=NULL)
}
\arguments{
    \item{model}{A TSmodel or object containing a TSmodel.}
    \item{data}{A TSdata object.}
    \item{y0}{initial condition ...}
}
\value{An integer.}
\details{
    For many time series models several starting data points are 
    required before output from the model can be calculated (or makes
    sense). This generic function extracts or calculates that 
    number of periods.
}

\seealso{
    \code{\link{TSmodel}}
}

\note{There is some redundancy between this and startShift which 
      should be cleaned up.}
\concept{DSE}
\keyword{ts}


