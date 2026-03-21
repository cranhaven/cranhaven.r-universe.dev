\name{SEMUPDATE}
\alias{SEMUPDATE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update lexicon
}
\description{
Update meaning lexicon on the basis of usage. Involves \code{NOUNDESEMANTICIZATION}, \code{VERBDESEMANTICIZATION}, \code{FUSE}, and \code{PERSONUPDATE}. Also, words that have become meaningless are replaced.
}
\usage{
SEMUPDATE(agentID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agentID}{
Pointer to agent whose lexicon is to be updated.
}
}
\value{
agent with updated lexicon
}
\author{
Sander Lestrade
}

\seealso{
\code{talk}
}
\examples{
FOUND()
SEMUPDATE(1)
}
\keyword{misc}
