\name{seriesNames.TSdata}
\alias{seriesNames.TSdata}
\alias{seriesNames.TSmodel}
\alias{seriesNames.TSestModel}
\alias{seriesNames<-.TSdata}
\alias{seriesNames<-.TSmodel}
\alias{seriesNames<-.TSestModel}

\title{Series Names Specific Methods}
\description{See the generic function description.}
\usage{
    \method{seriesNames}{TSdata}(x)
    \method{seriesNames}{TSmodel}(x)
    \method{seriesNames}{TSestModel}(x)
    
    \method{seriesNames}{TSdata}(x) <- value
    \method{seriesNames}{TSmodel}(x) <- value
    \method{seriesNames}{TSestModel}(x) <- value
}
\arguments{
    \item{x}{an object from which series names can be extracted or
             to which series names are to be assigned.}
    \item{value}{series names to be assigned to data.}
}

\seealso{ \code{\link[tframe]{seriesNames}} }

\concept{DSE}
\keyword{ts}

