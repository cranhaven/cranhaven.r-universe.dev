\name{SUMMARY}
\alias{SUMMARY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize simulation results
}
\description{
Summarize results of simulation
}
\usage{
SUMMARY()
}
%- maybe also 'usage' for other objects documented here.
\value{
List and plots
\item{generation}{generation of present agent}
\item{order}{word-order generalizations}
\item{topic}{topic-order generalizations}
\item{index}{verb-marker generalizations}
\item{person}{generalizations about role marking per person (e.g. first person undergoers should be marked)}
\item{actor}{actor-marking generalizations on the basis of meaning (e.g. all actors with a zero value on the first dimension should be marked)}
\item{undergoer}{undergoer-marking generalizations on the basis of meaning (e.g. all undergoer with a zero value on the first dimension should be marked)}
\item{markers}{which words were mostly used as markers}
\item{nounMarkerUse1}{proportion of role-marking in total}
\item{nounMarkerUse12}{proportion of role-marking of transitive events}
\item{first}{lexemes with first-person meaning}
\item{second}{lexemes with second-person meaning}
}
\author{
Sander Lestrade
}

\seealso{
\code{CHECKMARKER}, \code{HISTORY}
}
\examples{
\dontrun{
FOUND()
RUN(.0001)	#create results to summarize: first generation has to die
world$deathAge=10
DIE(1)
SUMMARY()
}
}
\keyword{misc}
