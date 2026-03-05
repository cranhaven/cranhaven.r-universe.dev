\name{ident}
\title{Create an Individual Identification Vector for a Point Process}
\alias{ident}
\usage{
ident(y, id)
}
\description{
\code{ident} creates a vector of length \code{sum(y)} by repeating the
values of individual identifiers for the times for use with
\code{\link[event]{ehr}}.
}
\arguments{
\item{y}{Vector of times.}
\item{id}{Vector of corresponding individual identifiers for who had
which sequence of times.}
}
\value{
\code{ident} creates a vector of length \code{sum(y)} by repeating the
values of individual identifiers for the times for use with
\code{\link[event]{ehr}}.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{bp}}, \code{\link[event]{ehr}},
\code{\link[event]{pp}}, \code{\link[event]{tccov}},
\code{\link[event]{tpast}}, \code{\link[event]{ttime}},
\code{\link[event]{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
i <- c(1,1,2,2)
id <- ident(y, i)
id
}
\keyword{manip}
