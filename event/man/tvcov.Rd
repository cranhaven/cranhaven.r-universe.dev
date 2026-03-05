\name{tvcov}
\title{Create a Vector of Time-varying Covariates for a Point Process}
\alias{tvcov}
\usage{
tvcov(y, x, tx)
}
\description{
\code{tvcov} creates a vector of length \code{sum(y)} of time-varying
covariates for use with \code{\link[event]{ehr}}. It may also be used to create
weight vectors.
}
\arguments{
\item{y}{Vector of times.}
\item{x}{Vector covariate.}
\item{tx}{Vector of times at which x changes.}
}
\value{
\code{tvcov} creates a vector of length \code{sum(y)} of time-varying
covariates for use with \code{\link[event]{ehr}}. It may also be used to create
weight vectors.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{ident}}, \code{\link[event]{pp}},
\code{\link[event]{tccov}}, \code{\link[event]{tpast}},
\code{\link[event]{ttime}}.
}
\examples{
y <- c(5,3,2,4)
x <- c(1,2,2,1,2,2,1)
tx <- c(2,3,1,2,2,2,2)
zcov <- tvcov(y, x, tx)
zcov
}
\keyword{manip}
