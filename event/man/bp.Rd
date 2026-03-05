\name{bp}
\title{Create a Vector of Cumulative Numbers of Previous Events
for a Point Process (Birth Processes)}
\alias{bp}
\usage{
bp(y, id, censor=1)
}
\arguments{
\item{y}{Vector of times.}
\item{id}{Vector of corresponding individual identifiers for who had
which sequence of times.}
\item{censor}{Vector of censoring indicators.}
}
\description{
\code{bp} creates a vector of length \code{sum(y)} of cumulative
numbers of previous events for each individual for use in fitting
birth processes with \code{\link{ehr}}. Add one if the process starts
at an event.
}
\value{
\code{bp} creates a vector of length \code{sum(y)} of cumulative
numbers of previous events for each individual for use in fitting
birth processes with \code{\link{ehr}}. Add one if the process starts
at an event.
}
\author{J.K. Lindsey}
\seealso{
\code{\link{ehr}}, \code{\link{ident}}, \code{\link{pp}}, \code{\link{tccov}},
\code{\link{tpast}}, \code{\link{ttime}}, \code{\link{tvcov}}.
}
\examples{
y <- c(5,3,2,4)
i <- c(1,1,2,2)
birth <- bp(y, i)
birth
}
\keyword{manip}
