\name{VERBMORPHOLOGY}
\alias{VERBMORPHOLOGY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret verbal morphology
}
\description{
Determine anaphoric reference of verb markers (either verb adpositions or suffixes).

}
\usage{
VERBMORPHOLOGY(hearerID, analysis)
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
If verb marker cannot be resolved anaphorically, it is reinterpret as a deictic argument.
}
\value{
Analysis (dataframe) with resolved reference of verb markers.
}
\author{
Sander Lestrade}

\seealso{
\code{INTERPRET}, \code{NOUNMORPHOLOGY}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
proposition$verb$topic=0; if('internal'\%in\%names(proposition)){proposition$internal$topic=0}
proposition$external$topic=1; proposition$external$recency=10
proposition=TOPICFIRST(1, proposition)
utterance=PRODUCE(1, proposition)
analysis=ANALYZE(2, utterance, situation)
grouping=GROUP(2, analysis)
for(i in 1:length(grouping)){
	if('verbAdposition'\%in\%grouping[[i]]$role){
		print(VERBMORPHOLOGY(2, grouping[[i]]))	
}	}
}
\keyword{misc}
