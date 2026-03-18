\name{golf}
\alias{golf}
\docType{data}
\title{ Number of Attempts and Successes at Golf Putts    }
\description{
    Number of attempts and successes of golf putts, by distance from  
    the hole, for a sample of professional golfers.
}
\usage{data(golf)}
\format{
  A data frame with 19 observations on the following 3 variables.
  \describe{
    \item{\code{distance}}{ Distance from hole in feet}
    \item{\code{n}}{number of attempts}
    \item{\code{y}}{number of successes}
  }
}
\details{
  This is used for an exercise on nonlinear modelling on page 515
  in the second edition.  
}
\examples{
data(golf)
names(golf)
comment(golf)
with(golf, plot(distance, y/n))
}
\keyword{datasets}
\concept{nonlinear}

