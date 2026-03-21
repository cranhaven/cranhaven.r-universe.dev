\name{WORDORDER}
\alias{WORDORDER}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use word order for interpretation
}
\description{
Use observed word-order tendencies for interpretation of role distribution. E.g., if agents were observed to come first mostly, assign first constituent agent role.
}
\usage{
WORDORDER(hearerID, analysis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent 
}
  \item{analysis}{
Analysis of utterance in which roles have to be determined.
}
}
\details{
To check if word order can be used, Yang's Tolerance principle is used.
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, possibly including role marking}
\item{internal}{representation of the internal argument, if identified, possibly including role marking}
\item{verb}{representation of the action argument, possibly including person indexing}
\item{target}{target event to be described}
}
\references{
Charles Yang (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.
}
\author{
Sander Lestrade
}

\seealso{
\code{INTERPRET}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
analysis=ANALYZE(2, utterance, situation)
WORDORDER(2, analysis)
population[[2]]$wordOrder[2,]$success=999	#makes AUV standard
population[[2]]$wordOrder
WORDORDER(2, analysis)
}
\keyword{misc}
