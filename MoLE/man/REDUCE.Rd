\name{REDUCE}
\alias{REDUCE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce length of expressions
}
\description{
Reduces length of frequently or recently used expressions by removing final character.
}
\usage{
REDUCE(speakerID, proposition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent.
}
  \item{proposition}{
Proposition with words whose forms may be reduced.
}
}
\details{
Reduction is an online production process only. It does not affect the lexical representation of the speaker (but cf. \code{EROSION})
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, possibly with shortened form}
\item{internal}{representation of the internal argument, if identified, possibly with shortened form}
\item{verb}{representation of the action argument, possibly with shortened form}
\item{target}{target event to be described}
}
\references{
Nettle, D. (1999). Linguistic diversity. New York: OUP.

Jurafsky, Daniel, Alan Bell, Michelle Gregory & William D. Raymond. 2001. "Probabilistic relations between words: Evidence from reduction in lexical production". In: J. Bybee and P. Hopper (eds), Frequency and the emergence of linguistic structure, 229-255. Amsterdam/Philadelphia. John Benjamins.
}
\author{
Sander Lestrade
}

\seealso{
\code{PREPARE}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
REDUCE(1, proposition)	#only effective if proposition includes frequently/recently used words
}
\keyword{misc}
