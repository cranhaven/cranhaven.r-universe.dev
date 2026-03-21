\name{TURN}
\alias{TURN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organize communicative turn 
}
\description{
Organize communicative turn in conversation. Involves generating a situation (\code{SITUATION}), developing an utterance (\code{PROPOSITION}, \code{PREPARE}, \code{PRODUCE}), interpreting the utterance (\code{INTERPRET}), and updating the lexicon (\code{FREQUPDATE}, \code{EROSION}).
}
\usage{
TURN(speakerID, hearerID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{hearerID}{
Pointer to hearer agent
}
}
\value{
Character string on screen, real output: agents with updated usage history.
}
\author{
Sander Lestrade
}

\seealso{
\code{TALK}
}
\examples{
FOUND()
TURN(1,2)
}
\keyword{misc}
