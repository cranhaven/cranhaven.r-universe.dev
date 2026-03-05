\name{tccov}
\title{Create a Vector of Time-constant Covariates for a Point Process}
\alias{tccov}
\usage{
tccov(y, x, id)
}
\description{
\code{tccov} creates a vector of length \code{sum(y)} of time-constant
covariates for use with \code{\link[event]{ehr}}. \code{id} must be numbered
consecutively. \code{x} must have one value for each distinct \code{id},
}
\arguments{
\item{y}{Vector of times.}
\item{x}{Vector covariate.}
\item{id}{Vector of corresponding individual identifiers for who had
which sequence of times.}
}
\value{
\code{tccov} creates a vector of length \code{sum(y)} of time-constant
covariates for use with \code{\link[event]{ehr}}. \code{id} must be numbered
consecutively. \code{x} must have one value for each distinct \code{id},
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{ident}}, \code{\link[event]{pp}},
\code{\link[event]{tpast}}, \code{\link[event]{ttime}},
\code{\link[event]{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
id <- c(1,1,2,2)
x <- c(5.2,3.1)
xcov <- tccov(y, x, id)
xcov
}
\keyword{manip}
