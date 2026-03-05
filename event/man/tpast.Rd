\name{tpast}
\title{Create a Vector of Times Past since Previous Events for a Point Process}
\alias{tpast}
\usage{
tpast(y)
}
\description{
\code{tpast} creates a vector of length \code{sum(y)} of times since the
immediately preceding event occurred for use with \code{\link[event]{ehr}}.
}
\arguments{
\item{y}{Vector of times.}
}
\value{
\code{tpast} creates a vector of length \code{sum(y)} of times since the
immediately preceding event occurred for use with \code{\link[event]{ehr}}.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{ident}}, \code{\link[event]{pp}},
\code{\link[event]{tccov}}, \code{\link[event]{ttime}},
\code{\link[event]{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
ptime <- tpast(y)
ptime
}
\keyword{manip}
