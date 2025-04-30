\name{forecast}
\Rdversion{1.1}
\alias{forecast}

\title{
Forecast future values
}

\description{
Forecast future values
}

\usage{
forecast(x, M, a, h = 10, opt = 2, alpha = 0.05)
}

\arguments{
\item{x}{Time series data}
\item{M}{Data model}
\item{a}{ARMA model}
\item{h}{Steps ahead}
\item{opt}{Display option (0 silent, 1 tabulate, 2 plot and tabulate)}
\item{alpha}{Level of significance}
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

All of the functions are inverted before the forecast
results are displayed.
}

\value{
Returns the following list invisibly.
\item{pred}{Predicted values}
\item{se}{Standard errors (not returned for data models with log)}
\item{l}{Lower bounds (95\% confidence interval)}
\item{u}{Upper bounds}
}

\seealso{
\code{\link{arma}}
\code{\link{Resid}}
\code{\link{test}}
}

\examples{
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
a = arma(e,1,1)
forecast(wine,M,a)
}
