\name{PROTOINTERPRETATION}
\alias{PROTOINTERPRETATION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Develop interpretation
}
\description{
Develop interpretation of an utterance using simple heuristics only (i.e., ignoring grammatical markers and tendencies). Applies if grammar either did not develop yet, or does not suffice for disambiguation.
}
\usage{
PROTOINTERPRETATION(hearerID, analysis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent
}
  \item{analysis}{
Analysis of the utterance to be interpreted (cf. \code{ANALYZE})
}
}
\details{
If only one role is unclear, it follows from simple reasoning (V has x and y role, A is x, then B must be y). If both roles are unclear, \code{TYPEMATCH} is used.
}
\value{
a dataframe, i.e. the analysis input in which the role column is updated.
}
\author{
Sander Lestrade
}

\seealso{
\code{VERBMORPHOLOGY}, \code{INTERPRET}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
(analysis=ANALYZE(2, utterance, situation))
PROTOINTERPRETATION(2, analysis)
}
\keyword{misc}
