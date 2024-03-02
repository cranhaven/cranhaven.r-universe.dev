\name{nstates}
\alias{nstates}
\alias{nstates.SS}
\alias{nstates.ARMA}
\alias{nstates.TSestModel}

\title{State Dimension of a State Space Model}
\description{Extract the state dimension of a state space model object.}
\usage{
    nstates(x)
    \method{nstates}{SS}(x)
    \method{nstates}{ARMA}(x)
    \method{nstates}{TSestModel}(x) 
 }
\arguments{
    \item{x}{Object of class TSmodel or TSestModel.}
}
\value{
    An integer indicating the state dimension.
}
\seealso{
    \code{\link{nseriesInput}}
}
\concept{DSE}
\keyword{ts}

