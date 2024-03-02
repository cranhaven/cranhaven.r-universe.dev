\name{nseriesInput}
\alias{nseriesInput}
\alias{nseriesInput.default}
\alias{nseriesInput.TSdata}
\alias{nseriesInput.SS}
\alias{nseriesInput.ARMA}
\alias{nseriesInput.TSestModel}

\alias{nseriesOutput}
\alias{nseriesOutput.default}
\alias{nseriesOutput.TSdata}
\alias{nseriesOutput.SS}
\alias{nseriesOutput.ARMA}
\alias{nseriesOutput.TSestModel}

\title{Number of Series in in Input or Output}
\description{Number of input or output series in a TSdata object.}
\usage{
    nseriesInput(x)
    \method{nseriesInput}{default}(x)
    \method{nseriesInput}{TSdata}(x)
    \method{nseriesInput}{SS}(x)
    \method{nseriesInput}{ARMA}(x)
    \method{nseriesInput}{TSestModel}(x)
    
    nseriesOutput(x)
    \method{nseriesOutput}{default}(x)
    \method{nseriesOutput}{TSdata}(x)
    \method{nseriesOutput}{SS}(x)
    \method{nseriesOutput}{ARMA}(x)
    \method{nseriesOutput}{TSestModel}(x)
}
\arguments{
    \item{x}{Object of class TSdata, TSmodel or TSestModel.}
}
\value{
    An integer indicating the number of series.
}
\seealso{
    \code{\link{seriesNamesInput}}
    \code{\link{seriesNamesOutput}}
}
\examples{
data("eg1.DSE.data", package="dse")
nseriesOutput(eg1.DSE.data) 
}
\concept{DSE}
\keyword{ts}

