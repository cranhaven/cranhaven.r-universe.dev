\name{NOUNDESEMANTICIZATION}
\alias{NOUNDESEMANTICIZATION}
\alias{VERBDESEMANTICIZATION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bleach word meaning
}
\description{
Update meaning representations of nouns/verbs on the basis of usage history.
}
\usage{
NOUNDESEMANTICIZATION(agent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
Agent whose lexicon is to be updated
}
}
\details{
Cf. Heine and Kuteva p.39: freq is epiphenomenon of extension, not cause; extension by combinatorial flexibility
8 is minimum freq from which Yang applies.
}
\value{
agent (with updated nominal representations)
}
\references{
Hopper, P. J. & Traugott, E. C. (2003). Grammaticalization. Cambridge: Cambridge University Press.

Heine, B. & Kuteva, T. (2007). The genesis of grammar. a reconstruction. Oxford: Oxford University Press.

Bybee, J. (2010). Language, usage and cognition. New York: Cambridge University Press.
}
\author{
Sander Lestrade
}

\seealso{
\code{SEMUPDATE}
}
\examples{
#only effective if usage history is non-empty
FOUND()
population[[1]]=NOUNDESEMANTICIZATION(population[[1]])	
population[[1]]=VERBDESEMANTICIZATION(population[[1]])	
}
\keyword{misc}
