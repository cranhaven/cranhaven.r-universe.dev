\name{TOPICCOPY}
\alias{TOPICCOPY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make anaphoric copy of topic 
}
\description{
Make verb-adjacent anaphoric copy of contrastive topic. Only applies if topics are moved to first position (after this generalization is made).
}
\usage{
TOPICCOPY(speakerID, proposition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{proposition}{
Proposition that agent is formulating
}
}
\details{
Anaphoric copies only need to distinguish topic from other argument for reestablished/non-continuous topics (cf. Givon)
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, possibly including role marking}
\item{internal}{representation of the internal argument, if identified, possibly including role marking}
\item{verb}{representation of the action argument, possibly including person indexing}
\item{target}{target event to be described}
}
\references{
T. Givon (1976), "Topic, pronoun, and grammatical agreement", In: C. Li (Ed.), Subject and topic, New York, etc.: Academic Press, Inc, 149-188.
}
\author{
Sander Lestrade
}

\seealso{
\code{TOPICFIRST}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
proposition$verb$topic=0; if('internal'\%in\%names(proposition)){proposition$internal$topic=0}
proposition$external$topic=1; proposition$external$recency=10
world$topicCopy=FALSE
proposition=TOPICFIRST(1, proposition)
PRODUCE(1, proposition)
proposition=TOPICCOPY(1, proposition)
PRODUCE(1, proposition)
}
\keyword{misc}
