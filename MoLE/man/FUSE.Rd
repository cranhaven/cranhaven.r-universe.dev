\name{FUSE}
\alias{FUSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fuse words
}
\description{
Fuses lexical items with frequently co-occuring markers into new lexical items
}
\usage{
FUSE(agent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
Agent whose lexical items are considered for fusion.
}
}
\details{
Words are only fused if the combination is used frequently enough (cf. \code{world}) and if meaning and form result of fusion is not in the lexicon already. Meaning of lexical item is overwritten for those meaning dimensions for which marker is specified only. Semantics of host is mixed with that of marker. 

}
\value{
No actual output: agent with updated lexicon
}
\references{
Bybee, J. (2010). Language, usage and cognition. New York: Cambridge University Press.

Bybee, J. L. (1985). Morphology. a study of the relation between meaning and form. Amsterdam/Philadelphia: John Benjamins.
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{SEMUPDATE}
}
\examples{
FOUND()
agent=population[[1]]
agent$collostructions$flag[1,]$N=agent$nouns$ID[nrow(agent$nouns)-1]
agent$collostructions$flag[1,]$marker=agent$nouns$ID[nrow(agent$nouns)]
agent$collostructions$flag[1,]$frequency=100
agent$nouns[nrow(agent$nouns),]$nounMarker=100
agent$nouns[nrow(agent$nouns)-1,]$person=1
agent$nouns[nrow(agent$nouns),6:9]=NA
agent$collostructions$flag
tail(agent$nouns)

agent=FUSE(agent)
agent$collostructions$flag
tail(agent$nouns)
}
\keyword{misc}
