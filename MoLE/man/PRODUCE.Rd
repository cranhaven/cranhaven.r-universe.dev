\name{PRODUCE}
\alias{PRODUCE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce utterance
}
\description{
Turns proposition into actual utterance. 
}
\usage{
PRODUCE(speakerID, prep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{prep}{
Proposition to be uttered
}
}
\details{
Internal markers are produced closest to verb (cf. Dryer); not exploited by hearer.
}
\value{
Character string
}
\references{
Matthew S. Dryer. 2013. Order of Subject, Object and Verb. In: Dryer, Matthew S. & Haspelmath, Martin (eds.) The World Atlas of Language Structures Online.
}
\author{
Sander Lestrade
}

\seealso{
\code{TURN}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
PRODUCE(1, proposition)
}
\keyword{misc}
