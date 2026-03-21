\name{PROCREATE}
\alias{PROCREATE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate new generation of agents
}
\description{
Generate new generation of agents if (to be) parent generation is old enough. 
}
\usage{
PROCREATE(speakerID, hearerID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to first parent
}
  \item{hearerID}{
Pointer to second parent
}
}
\details{
New generation is mix of vocabularies of parents (if \code{world$crossover} is T), with emptied usage histories. Agents procreate after number of utterances specified by \code{world$procreationAge}. Meanings of words that have not been used by their parents are modified slightly.
}
\value{
Set of new agents
}
\author{
Sander Lestrade
}

\seealso{
\code{TALK}
}
\examples{
FOUND()
population[[1]]$age=population[[2]]$age=world$procreationAge*world$deathAge+1
PROCREATE(1,2)
}
\keyword{misc}
