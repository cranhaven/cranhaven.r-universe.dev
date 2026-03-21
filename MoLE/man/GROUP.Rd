\name{GROUP}
\alias{GROUP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group words into constituents
}
\description{
Determines each possible constituent ordering (assuming adjacency) of an utterance. Given A B C V, in which V is identified as the verb, B could be a marker of A, or C could be a marker of B.
}
\usage{
GROUP(hearerID, analysis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to the hearer agent
}
  \item{analysis}{
Analysis of the utterance in which the individual lexemes have been determined and the verb has been identified.
}
}
\details{
VerbAdpositions are for topic cross reference only, and may be put on top of verb suffixes. 
VerbAdpositions are reanalyzed as verbSuffix if index=TRUE and no other verb suffixes (then no proper suffix was available)
Only non-local-person noun markers, to be removed if possessive marking is modelled.
}
\value{
list of all possible groupings
\item{[[1]]}{First possible grouping analysis}
\item{[[2]]}{Second possible grouping analysis, if possible, etc.}
}
\author{
Sander Lestrade
}

\seealso{
\code{INTERPRET}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
utterance=paste(utterance, unlist(strsplit(utterance, ' '))[1])
analysis=ANALYZE(2, utterance, situation)
GROUP(2, analysis)
}
\keyword{misc}
