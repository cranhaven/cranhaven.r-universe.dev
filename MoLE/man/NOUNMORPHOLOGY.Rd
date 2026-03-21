\name{NOUNMORPHOLOGY}
\alias{NOUNMORPHOLOGY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret nominal morphology
}
\description{
Use noun markers to determine event-role distribution (i.e., who is actor and who is undergoer).
}
\usage{
NOUNMORPHOLOGY(hearerID, analysis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent who's developing an analysis
}
  \item{analysis}{
Analysis of utterance (result of \code{ANALYZE}) in which roles have to be determined.
}
}
\details{
Marker overrules suffix with same host
Future work: allow for oblique roles.
}
\value{
Analysis (dataframe) with roles assigned on the basis of nominal markers.
}
\author{
Sander Lestrade
}

\seealso{
\code{INTERPRET}, \code{VERBMORPHOLOGY}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
prep=PREPARE(1, proposition, situation)
utterance=PRODUCE(1, prep)
analysis=ANALYZE(2, utterance, situation)
grouping=GROUP(2, analysis)
(analysis=NOUNMORPHOLOGY(2, grouping[[length(grouping)]]))
analysis$role
#repeat if no nounAdposition is identified.
}
\keyword{misc}
