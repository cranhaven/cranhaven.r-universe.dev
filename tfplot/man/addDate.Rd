\name{addDate}
\alias{addDate}
\title{Add Periods to a Date}
\description{
    Add periods to two element start date of given frequency to give a
    new date. NULL periods is treated as 0.
}
\usage{
    addDate(date, periods, freq)
}
\arguments{
    \item{date}{A two element date as used by tsp i.e c(year, period).}
    \item{periods}{A number of periods.}
    \item{freq}{The number of periods in a year.}
}
\value{A two element date.}

\seealso{
    \code{\link{tfExpand}}
}
\examples{
    addDate(c(1998,1), 20, 12)
}

\note{A useful utility not strictly part of tframe.}

\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


