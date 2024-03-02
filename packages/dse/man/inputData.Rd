\name{inputData}
\alias{inputData}
\alias{inputData.default}
\alias{inputData.TSdata}
\alias{inputData.TSestModel}
\alias{inputData<-}
\alias{inputData<-.default}
\alias{inputData<-.TSdata}

\alias{outputData}
\alias{outputData.default}
\alias{outputData.TSdata}
\alias{outputData.TSestModel}
\alias{outputData<-}
\alias{outputData<-.default}
\alias{outputData<-.TSdata}

\title{TSdata Series}
\description{Extract or set input or output series in a TSdata object.}
\usage{
    inputData(x, series=seqN(nseriesInput(x)))
    \method{inputData}{default}(x, series=seqN(nseriesInput(x)))
    \method{inputData}{TSdata}(x,  series=seqN(nseriesInput(x)))
    \method{inputData}{TSestModel}(x, series=seqN(nseriesInput(x)))

    outputData(x, series=seqN(nseriesOutput(x)))
    \method{outputData}{default}(x, series=seqN(nseriesOutput(x)))
    \method{outputData}{TSdata}(x,  series=seqN(nseriesOutput(x)))
    \method{outputData}{TSestModel}(x, series=seqN(nseriesOutput(x)))

    inputData(x)  <- value
    outputData(x) <- value
}
\arguments{
    \item{x}{object of class TSdata.}
    \item{value}{a time series matrix.}
    \item{series}{vector of strings or integers indicating the series to select.}
}
\value{
The first usages returns the input or output series.
The second usages assigns the input or output series.
}
\seealso{
    \code{\link{TSdata}}
    \code{\link[tframe]{selectSeries}}
}
\examples{
data("eg1.DSE.data", package="dse")
outputData(eg1.DSE.data) 
}
\concept{DSE}
\keyword{ts}

