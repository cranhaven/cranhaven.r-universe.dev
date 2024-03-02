\name{seriesNamesInput}
\alias{seriesNamesInput}
\alias{seriesNamesInput.TSdata}
\alias{seriesNamesInput.TSmodel}
\alias{seriesNamesInput.TSestModel}
\alias{seriesNamesInput<-}
\alias{seriesNamesInput<-.TSdata}
\alias{seriesNamesInput<-.TSmodel}
\alias{seriesNamesInput<-.TSestModel}


\alias{seriesNamesOutput}
\alias{seriesNamesOutput.TSdata}
\alias{seriesNamesOutput.TSmodel}
\alias{seriesNamesOutput.TSestModel}
\alias{seriesNamesOutput<-}
\alias{seriesNamesOutput<-.TSdata}
\alias{seriesNamesOutput<-.TSmodel}
\alias{seriesNamesOutput<-.TSestModel}

\title{TSdata Series Names}
\description{Extract or set names of input or output series in a TSdata object.}
\usage{
    seriesNamesInput(x)
    \method{seriesNamesInput}{TSdata}(x)
    \method{seriesNamesInput}{TSmodel}(x)
    \method{seriesNamesInput}{TSestModel}(x)

    seriesNamesOutput(x)
    \method{seriesNamesOutput}{TSdata}(x)
    \method{seriesNamesOutput}{TSmodel}(x)
    \method{seriesNamesOutput}{TSestModel}(x)

    seriesNamesInput(x)  <- value
    seriesNamesOutput(x) <- value
}
\arguments{
    \item{x}{Object of class TSdata, TSmodel or TSestModel.}
    \item{value}{value to be assigned to object.}
}
\value{
The first usages gives a vector of strings with the series names.
The second usages assigns a vector of strings to be the series names of data.
}
\seealso{
    \code{\link[tframe]{seriesNames}}
}
\examples{
data("eg1.DSE.data", package="dse")
seriesNamesOutput(eg1.DSE.data) 
}
\concept{DSE}
\keyword{ts}


