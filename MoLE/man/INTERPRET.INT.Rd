\name{INTERPRET.INT}
\alias{INTERPRET.INT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Develop an interpretation
}
\description{
Translates utterance analysis into a proposition (i.e., an interpretation) and determines match with ongoing events.
}
\usage{
INTERPRET.INT(hearerID, analysis, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to the hearer agent that is interpreting an analysis
}
  \item{analysis}{
The analysis to be translated
}
  \item{situation}{
Situation in which utterance is interpreted.
}
}
\details{
\code{INTERPRET.INT} works internal to \code{INTERPRET}, which compares the interpretations of the different possible analyses.
VerbAdpositions overrule verbSuffixes... 
}
\value{
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument, if identified}
\item{verb}{representation of the action argument}
\item{target}{target event identified on the basis of interpretation, including matching scores}
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{INTERPRET}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
analysis=ANALYZE(2, utterance, situation)
(analysis=PROTOINTERPRETATION(2, analysis))
INTERPRET.INT(2, analysis, situation)
}
\keyword{misc}
