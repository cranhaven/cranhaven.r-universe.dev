\name{changeTSrepresentation}
\alias{changeTSrepresentation}

\title{Change the class of a (Multivariate) Time Series}
\description{Change the class of a (Multivariate) Time Series}

\usage{
    changeTSrepresentation (x, newRepresentation)
}

\arguments{
    \item{x}{a multivariate time series object.}
    \item{newRepresentation}{function or character string indicating the
          function to use to change the time series class.} 
}
\value{a multivariate time series object.}
\details{
This function attempts to convert the time series to the new class using 
  \code{newRepresentation}.

If the representation is a character string then it is applied using \code{do.call(TSrepresentation, list(x, dates))}
where \code{x} is the time series (matrix) to be return and dates are
determined by \code{as.Date(time(x))} where \code{x} is the default representation of the data. If representation is not a character string 
then it should be
a function and is applied using \code{TSrepresentation(x, dates)}
}

\keyword{utilities}
\keyword{ts}
