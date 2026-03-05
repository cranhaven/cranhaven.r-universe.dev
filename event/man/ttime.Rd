\name{ttime}
\title{Create a Vector of Total Time Elapsed for each Individual for a
Point Process}
\alias{ttime}
\usage{
ttime(y, id)
}
\description{
\code{ttime} creates a vector of length \code{sum(y)} of times since
each individual began for use with \code{\link[event]{ehr}}.
}
\arguments{
\item{y}{Vector of times.}
\item{id}{Vector of corresponding individual identifiers for who had
which sequence of times.}
}
\value{
\code{ttime} creates a vector of length \code{sum(y)} of times since
each individual began for use with \code{\link[event]{ehr}}.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{ident}}, \code{\link[event]{pp}},
\code{\link[event]{tccov}}, \code{\link[event]{tpast}},
\code{\link[event]{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
id <- c(1,1,2,2)
itime <- ttime(y, id)
itime
}
\keyword{manip}
