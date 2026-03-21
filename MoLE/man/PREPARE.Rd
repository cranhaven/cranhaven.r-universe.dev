\name{PREPARE}
\alias{PREPARE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare a proposition for production
}
\description{
Prepare a proposition for production by checking if it will be intelligible and applying generalizations.
}
\usage{
PREPARE(speakerID, proposition, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{proposition}{
Proposition that is prepared for production
}
  \item{situation}{
Situation in which proposition is to be uttered.
}
}
\details{
\code{PREPARE} involves a number of subroutines: If role distribution is unclear, agents checks if this can be resolved with pronominal case forms and if not with noun markers (\code{CHECKSUCCESS}). If referential expression is too weak, stronger expressions are used (\code{REFCHECK}). If generalizations such as PutAgentFirst or IndexFirstPerson are made, they are applied (\code{GENERALIZE}). If words are frequently used, their forms are reduced (\code{REDUCE}). Etc.
Ingredients of proposition are ordered by activation before other principles apply
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, possibly including role marking}
\item{internal}{representation of the internal argument, if identified, , possibly including role marking}
\item{verb}{representation of the action argument, possibly including person indexing}
\item{target}{target event to be described}
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
PREPARE(1, proposition, situation) 
#result need not be different from simple proposition, 
#depends on generalizations and typing scores
}
\keyword{misc}
