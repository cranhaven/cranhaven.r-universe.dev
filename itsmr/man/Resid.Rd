\name{Resid}
\Rdversion{1.1}
\alias{Resid}

\title{
Compute residuals
}

\description{
Compute residuals
}

\usage{
Resid(x, M = NULL, a = NULL)
}

\arguments{
\item{x}{Time series data}
\item{M}{Data model}
\item{a}{ARMA model}
}

\details{
The data model can be \code{NULL} for none.
Otherwise \code{M} is a vector of function names and arguments.

Example:

\code{M = c("log","season",12,"trend",1)}

The above model
takes the log of the data, then subtracts a seasonal component
of period 12, then subtracts a linear trend component.

These are the available functions:
\tabular{ll}{
\code{diff}\tab
Difference the data. Has a single argument, the lag.
\cr
\code{hr}\tab
Subtract harmonic components. Has one or more
arguments, each specifying the number of observations per harmonic.
\cr
\code{log}\tab
Take the log of the data, has no arguments.
\cr
\code{season}\tab
Subtract a seasonal component.
Has a single argument, the number of observations per season.
\cr
\code{trend}\tab
Subtract a trend component.
Has a single argument, the order of the trend
(1 linear, 2 quadratic, etc.)
}

At the end of the model there is an implicit subtraction of the mean operation.
Hence the resulting time series always has zero mean.
}

\value{
Returns a vector of residuals the same length as \code{x}.
}

\seealso{
\code{\link{test}}
}

\examples{
M = c("log","season",12,"trend",1)
e = Resid(wine,M)

a = arma(e,1,1)
ee = Resid(wine,M,a)
}
