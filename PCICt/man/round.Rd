\name{round.PCICt}
\alias{round.PCICt}
\title{round.PCICt}
\description{
  Round PCICt objects to the nearest second/minute/hour/day
}
\usage{
\method{round}{PCICt}(x, digits = c("secs", "mins", "hours", "days"))
}
\arguments{
  \item{x}{Dates to be rounded.}
  \item{digits}{Unit to round the dates to.}
}

\details{
  round.PCICt rounds the dates in the \code{x} argument to the nearest
  second/minute/hour/day, as specified by the poorly named \code{digits}
  argument.
}

\value{
  The dates in \code{x}, rounded to the nearest second/minute/hour/day.
}

\examples{
## Convert strings to PCICt objects, on a 360 day calendar
x <- as.PCICt(c("1961-02-30 12:00:00", "1962-03-24 12:34:56"), cal="360")

## Round them to the nearest hour
x.hour <- round(x, "hours")

## Round them to the nearest day
x.day <- round(x, "days")
}

\seealso{
  \code{\link{trunc}}
}