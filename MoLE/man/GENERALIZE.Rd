\name{GENERALIZE}
\alias{GENERALIZE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply linguistic generalizations
}
\description{
Checks whether the previous use of certain constructions or word orders reaches a generalization threshold. If so, the construction will be used independently from its current communicative value. 
}
\usage{
GENERALIZE(speakerID, proposition, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker who's considering the use standard use of a construction
}
  \item{proposition}{
The proposition to which the construction applies
}
  \item{situation}{
The communicative situation in which the utterance is made
}
}
\details{
For the generalization threshold, Yang's Tolerance principle is used, which says that the number of exceptions to a rule for it to be applied/maintained/stipulated has to be below n/log(n), with n being the number of instances the rule (could have) applied. N exceptions should minimally be 4(=8/ln(8)) for Yang to make sense.
Generalizations are checked, for word order first (in which grammatical order is overruled by topic generalizations), then for marking (since solutionMethod for marking sometimes dependent on word order). 
Noun marking first checked at general level, then for more specific dimensions of semantic role.
"values=\-values[1:length(speaker$usageHistory$flag\-[[firstArgument$semRole]]$value)]" is necessary for economically stored resurrected agents (if world$saveAll=F and their behavior is checked)
Third-person pronoun are only used if single third-person referent in situation.
}
\value{
A list: the proposition, possibly in a generalized form.
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument, if identified}
\item{verb}{representation of the action argument}
\item{target}{target event to be described}
}
\references{
Charles Yang (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{PREPARE}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
GENERALIZE(1, proposition, situation)
population[[1]]$wordOrder[3,2:3]=9999
population[[1]]$wordOrder
GENERALIZE(1, proposition, situation)
}
\keyword{misc}
