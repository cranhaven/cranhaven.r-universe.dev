\name{TSdata.object}
\alias{TSdata.object}
\title{time series data object}
\description{
Class \code{TSdata} of time series data objects for use with TSmodels.
}

\section{Generation}{
This class of objects is returned by specific methods of the 
function TSdata or can be built according to the description below.
}

\section{Methods}{
The \code{TSdata} class of objects has methods for the generic
functions \code{print, plot, start, end, ..., testEqual, seriesNames,
seriesNamesInput, seriesNamesOutput}
Also, the function \code{is.TSdata} is supported.
}

\section{Inheritance}{
Other data classes inherit from the class \code{TSdata}.}

\section{Structure}{
Objects are a list with class the most general class \code{TSdata}. The
native form for this package has elements \code{input} and
\code{output}. Any other elements are ignored. \code{input} and
\code{output} are matrices (or tframe or time series matrices) of the
input and output data, with each series in a column.  It is possible to 
populate this structure directly from a time series database. See the 
\pkg{TSdbi} package for more details.
}

\seealso{
\code{\link{TSdata}},
\code{\link{TSmodel}},
\code{\link{TSestModel.object}}
}
\concept{DSE}
\keyword{ts}

