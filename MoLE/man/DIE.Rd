\name{DIE}
\alias{DIE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kill agents
}
\description{
After a prespecified number of utterances (and after having given birth to a new generation of speakers), agents are removed from the (actively speaking) population. Death agents are stored in the \code{graveyard} for later inspection.
}
\usage{
DIE(agentID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agentID}{
Pointer to agent whose death is considered.
}
}
\details{
\code{DIE} is called at the end of each turn, but only applies if the agent is old enough.
}
\value{
New entry in \code{graveyard}.
}
\author{
Sander Lestrade
}
\examples{
FOUND()
population[[1]]$age=world$deathAge+1
DIE(1)
}
\keyword{misc}
