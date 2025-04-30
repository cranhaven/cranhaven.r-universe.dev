\name{arar}
\Rdversion{1.1}
\alias{arar}

\title{
Forecast using ARAR algorithm
}

\description{
Forecast using ARAR algorithm
}

\usage{
arar(y, h = 10, opt = 2)
}

\arguments{
\item{y}{Time series data}
\item{h}{Steps ahead}
\item{opt}{Display option (0 silent, 1 tabulate, 2 plot and tabulate)}
}

\value{
Returns the following list invisibly.
\item{pred}{Predicted values}
\item{se}{Standard errors}
\item{l}{Lower bounds (95\% confidence interval)}
\item{u}{Upper bounds}
}

\seealso{
\code{\link{forecast}}
}

\examples{
arar(airpass)
}
