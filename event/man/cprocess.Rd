\name{cprocess}
\title{Plot Counting Process Data}
\alias{cprocess}
\usage{
cprocess(times=NULL, events=NULL, number=FALSE, lty=NULL, xlim=NULL,
	ylim=NULL, xlab="Time", ylab="Counts", ...)
}
\description{
\code{cprocess} plots the cumulative number of events (the counting
process) over time for each vector in the list. One or both of
\code{times} and \code{events} can be supplied.
}
\arguments{
\item{times}{Vector of times between events, a list of such
vectors, or a \code{repeated} object.}
\item{events}{Vector of counts of events, a list of such vectors, or a
\code{repeated} object.}
\item{number}{If TRUE, the counting processes are numbered
consecutively on the graph.}
\item{ylab}{Plotting control options.}
\item{xlab}{Plotting control options.}
\item{xlim}{Plotting control options.}
\item{ylim}{Plotting control options.}
\item{lty}{Plotting control options.}
\item{...}{Plotting control options.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link[rmutil]{read.list}}, \code{\link[rmutil]{rmna}}.
}
\examples{
times <- rgamma(20,2,scale=4)
cprocess(times)
}
\keyword{hplot}
