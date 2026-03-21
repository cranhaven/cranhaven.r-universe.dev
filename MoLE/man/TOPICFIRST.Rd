\name{TOPICFIRST}
\alias{TOPICFIRST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Put topic in first position
}
\description{
Put topic of the utterance in first position if such a tendency was observed in and therefore generalization was made on the basis of previous utterances.
}
\usage{
TOPICFIRST(speakerID, proposition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{proposition}{
Proposition in which topic argument is to be moved}
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, possibly including role marking}
\item{internal}{representation of the internal argument, if identified, possibly including role marking}
\item{verb}{representation of the action argument, possibly including person indexing}
\item{target}{target event to be described},
in which the topic argument is put first
}
\references{
Tomlin, R. S. (1986). Basic word order:  Functional principles (Vol. 13).  Routledge

Ferrer-i-Cancho,  R.   (2014).   Why  might  SOV be  initially  preferred  and  then lost or recovered?  a theoretical framework.  In: Proceedings of the 10th international conference (evolang10), pp. 66-73.

Bates,  E.,  &  MacWhinney,  B.  (1987).  Competition,  variation,  and  language learning. Mechanisms of language acquisition, 157-193.
}
\author{
Sander Lestrade
}

\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
PRODUCE(1, proposition)
proposition=TOPICFIRST(1, proposition)
PRODUCE(1, proposition)
}
\keyword{misc}
