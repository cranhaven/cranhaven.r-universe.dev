\name{PROPOSITION}
\alias{PROPOSITION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Develop initial proposition
}
\description{
Develop initial proposition that consists of the expressions that refer to the participants of the event to be expressed (both objects and action). Proposition will be elaborated upon in later stages of the production processs, and word  may be replaced later if they turn out to be insufficient (cf. \code{PREPARE}).
}
\usage{
PROPOSITION(speakerID, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{situation}{
Situation with target event to be referred to and number of distractor event
}
}
\details{
Words are ranked on the basis of a combination of semantic match (how well does word refer to its participant), frequency, and recency (cf. \code{CANDIDATEORDER}). The first word to be sufficiently distinctive is selected for expression.
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument}
\item{verb}{representation of the action argument}
\item{target}{target event to be described}
}
\author{
Sander Lestrade}

\seealso{
\code{TURN}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
}
\keyword{misc}
