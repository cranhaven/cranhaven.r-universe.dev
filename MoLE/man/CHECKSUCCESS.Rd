\name{CHECKSUCCESS}
\alias{CHECKSUCCESS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine expected communicative success
}
\description{
Check whether the hearer is likely to arrive at the intended role distribution and elaborate if not through explicit role marking.
}
\usage{
CHECKSUCCESS(speakerID, proposition, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to the speaker agent
}
  \item{proposition}{
The proposition that is to be conveyed
}
  \item{situation}{
The situation in which the event that the proposition refers to is embedded.
}
}
\details{
Elaboration is necessary if best typing match leads to wrong distribution of roles, but not if (one of) the roles are marked one way or another
First try if indexes are informative, next try appropriate pronominal case form, then check if word order is informative (if generalizations are made)
N exceptions should minimally be 4(=8/ln(8)) for Yang's tolerance principle.
}
\value{
A list, i.e. a checked and possibly elaborated proposition.
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument, if identified}
\item{verb}{representation of the action argument}
\item{target}{target event to be described}
}
\references{
Levelt, W.J.M. 1983. "Monitoring and self-repair in speech". Cognition 14.41-104

Hurford, J. R. 1989. "Biological evolution of the saussurean sign as a component of the language acquisition device". Lingua 77:2.187-222.

Steels, L. 2003. "Language re-entrance and the inner voice". Journal of Consciousness Studies 10:4-5.173-185.

Blutner, Reinhard, Helen de Hoop \& Petra Hendriks. 2006. Optimal Communication. Stanford: CSLI.

Charles Yang (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.
}
\author{
Sander Lestrade
}
\note{
Often, the interpretation of an utterance follows automatically by world knowledge in which case no explicit marking is necessary (e.g. "man book read"). Role marking is only necessary if participants qualify for both roles equally well (e.g. "man woman see") or if a participant qualifies better for another role and outperforms the intended performer in this (e.g. "man pig kill", in which the pig is the intended actor).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{PREPARE}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
CHECKSUCCESS(1, proposition, situation)
}
\keyword{misc}
