\name{INTERPRET}
\alias{INTERPRET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret utterance
}
\description{
Determines the best interpretation of an utterance given the situational context. Compares different interpretations if multiple analyses are possible and chooses most likely one given context.
}
\usage{
INTERPRET(hearerID, utterance, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to the hearer agent
}
  \item{utterance}{
The utterance to be interpreted
}
  \item{situation}{
Set of events in which utterance was used
}
}
\details{
  #first use explicit role marking
  #then word order (if still necessary)
  #then verb morphology (idem)
}
\value{
interpretation, i.e. a list:
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
\code{TURN}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
INTERPRET(2, utterance, situation)
}
\keyword{misc}
