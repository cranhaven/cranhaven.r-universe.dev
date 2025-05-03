\name{Ops.PCICt}
\alias{Ops.PCICt}
\alias{+.PCICt}
\alias{-.PCICt}
\alias{[.PCICt}
\alias{[<-.PCICt}
\title{Ops.PCICt}
\description{
  These functions implement subtraction, addition, indexing, and index
  assignment operations as in POSIXct.
}

\value{
  A PCICt object with the given operations performed.
}

\examples{
## Create a list of PCICt of length 2 with a 365-day calendar
x <- as.PCICt(c("1961-09-02", "1963-02-01"), cal="365_day")

## Look at the difference between the two elements of x
y <- x[1] - x[2]

## Change the first element of x
x[1] <- as.PCICt("1962-09-02", cal="365_day")
}

\keyword{ts}