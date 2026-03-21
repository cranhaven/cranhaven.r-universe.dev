\name{AGENTFIRST}
\alias{AGENTFIRST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Actor argument first
}
\description{
Reorganizes constituents of an utterance such that actor is put in sentence-initial position. Only applies if corresponding word-order generalization has been made.
}
\usage{
AGENTFIRST(proposition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proposition}{
Proposition of which the constituents are reordered. 
}
}
\details{
Applies to intransitives too, which may not be desirable. 
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument, if identified}
\item{verb}{representation of the action argument}
\item{target}{target event to be described}
}
\references{
Matthew S. Dryer. 2013. Order of Subject, Object and Verb. In: Dryer, Matthew S. & Haspelmath, Martin (eds.) The World Atlas of Language Structures Online. Leipzig: Max Planck Institute for Evolutionary Anthropology. (Available online at http://wals.info/chapter/81, Accessed on 2017-05-24.)  
}
\author{
Sander Lestrade
}

\seealso{
\code{GENERALIZE}
}
\examples{
FOUND()
situation=SITUATION(1)
(proposition=PROPOSITION(1, situation))
AGENTFIRST(proposition)
}
\keyword{misc}
