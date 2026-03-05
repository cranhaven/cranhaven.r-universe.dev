\name{pp}
\title{Create a Point Process Vector from Times between Events}
\alias{pp}
\usage{
pp(y, censor=1)
}
\description{
\code{pp} creates a vector of length \code{sum(y)} of zeroes with a one
at the end of each uncensored time interval for use with
\code{\link[event]{ehr}}.
}
\arguments{
\item{y}{Vector of times.}
\item{censor}{Vector of censoring indicators.}
}
\value{
\code{pp} creates a vector of length \code{sum(y)} of zeroes with a one
at the end of each uncensored time interval for use with
\code{\link[event]{ehr}}.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{ident}}, \code{\link[event]{tccov}},
\code{\link[event]{tpast}}, \code{\link[event]{ttime}},
\code{\link[event]{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
py <- pp(y)
py
}
\keyword{manip}
