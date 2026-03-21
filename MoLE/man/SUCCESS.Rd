\name{SUCCESS}
\alias{SUCCESS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine communicative success
}
\description{
Determine communicative success by comparing intention of speaker (proposition) and interpretation of hearer.
}
\usage{
SUCCESS(proposition, interpretation, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proposition}{
Intended/speaker meaning
}
  \item{interpretation}{
Interpretation/hearer meaning
}
  \item{situation}{
Contextual situation in which communication took place.
}
}
\details{
If there are no distractor events ongoing, success is determined by comparing the speaker and hearer meanings; otherwise, communication is successful if the same target event is selected.
}
\value{
Logical: 1 for success; 0 for failure
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
(proposition=PROPOSITION(1, situation))
utterance=PRODUCE(1, proposition)
(interpretation=INTERPRET(2, utterance, situation))
SUCCESS(proposition, interpretation, situation)
}
\keyword{misc}
