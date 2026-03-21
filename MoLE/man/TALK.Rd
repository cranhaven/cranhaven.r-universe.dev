\name{TALK}
\alias{TALK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Let agents talk
}
\description{
Sample two agents and let them talk with each other.
}
\usage{
TALK(nTurns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nTurns}{
Number of communicative turns a conversation last before new agents are selected for communication.
}
}
\details{
Young agents are less likely to talk with each other.
}
\value{
On screen conversation. (Underlying update of usage history of talking agents.)
}
\author{
Sander Lestrade
}

\seealso{
\code{RUN}
}
\examples{
FOUND()
\dontrun{TALK(4)}
}
\keyword{misc}
