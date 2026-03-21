\name{PERSONUPDATE}
\alias{PERSONUPDATE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjust person value
}
\description{
Adjust person value of noun from third to local (first or second) if it has been recruited frequently to express local reference.
}
\usage{
PERSONUPDATE(agent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
Agent whose nominal lexicon is considered.
}
}
\details{
Multiple verb markers with same person are taken care of too. Redundant local pronouns and indexes are removed.
}
\value{
Agent (with updated lexicon)
}
\references{
Zeevat, Henk. 2007. "Simulating recruitment in evolution". Cognitive Foundations of Interpretation ed. by G. Bouma, I. Kraemer & J. Zwarts, 175-194. Amsterdam: Royal Netherlands Academy of Arts and Sciences.
}
\author{
Sander Lestrade
}

\seealso{
\code{SEMUPDATE}
}
\examples{
FOUND()
population[[1]]=PERSONUPDATE(population[[1]]) #only effective if pronouns have been recruited
}
\keyword{misc}
