\name{SITUATION}
\alias{SITUATION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create situational context
}
\description{
Create situational context that consists of set of events among which the target event to be described.
}
\usage{
SITUATION(speakerID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent whose concepts are used to create situation.
}
}
\details{
Events are generated on the basis of agents' world knowledge. In principle, qualified participants are more likely than unqualified ones (e.g. books are read, not eaten; cf. \code{world$roleNoise}; \code{world$referenceNoise}).
Local person always known, so if world$local==T, oddsNew for Dahl numbers are adjusted.
Situations with multiple events are more likely than situations with single event.
Locals are animate.
If none of the candidates qualifies argument criterium, only recency is used for topichood (cf. DuBois: preference for actor topic)
}
\value{
dataframe with sets of vectors that specify actions and actors, and if present undergoer participants. 
}
\references{
John W. DuBois (1987), The discourse basis of ergativity. Language 63 (4)
}
\author{
Sander Lestrade
}

\seealso{
\code{TURN}
}
\examples{
FOUND()
SITUATION(1)
}