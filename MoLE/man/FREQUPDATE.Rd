\name{FREQUPDATE}
\alias{FREQUPDATE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update usage numbers
}
\description{
Update frequency numbers in lexicon and usage history.
}
\usage{
FREQUPDATE(agentID, meaning, success)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agentID}{
pointer to agent whose numbers are to be updated
}
  \item{meaning}{
Meaning (proposition or interpretation) on the basis of whose constituents the numbers in the usage history and lexicon have to be updated.
}
  \item{success}{
Logical for success of conversational turn (as number of successful uses are kept track of)
}
}
\details{
Difference is made between local and third-person pronouns. +1 at the end for log operation and to prevent division by zero}
\value{
No actual output: updated usage history and lexicon
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
population[[1]]$nouns[population[[1]]$nouns$ID\%in\%c(proposition$external$ID),]
population[[1]]$wordOrder
FREQUPDATE(1, proposition, success=1)
population[[1]]$nouns[population[[1]]$nouns$ID\%in\%c(proposition$external$ID),]
population[[1]]$wordOrder
}
\keyword{misc}
