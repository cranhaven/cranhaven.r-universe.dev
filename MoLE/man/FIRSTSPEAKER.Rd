\name{FIRSTSPEAKER}
\alias{FIRSTSPEAKER}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create founding agent
}
\description{
Creates first agent of a lineage which only consists of a conventional symbolic lexicon (and the infrastructure to count word uses). 
}
\usage{
FIRSTSPEAKER()
}
%- maybe also 'usage' for other objects documented here.
\details{
Start with 4 for log operations later on. Only with minimally 4 exceptions, Tolerance threshold is minority indeed
}
\value{
\item{age}{age of agent at birth=0}
\item{generation}{generation of agent}
\item{fertile}{logical for fertility of agent (1 until procreated)}
\item{semupdate}{logical that says that whether agent has updated its semantics already (0 at birth; cf. \code{SEMUPDATE})}
\item{verbs}{verbal lexicon}
\item{nouns}{nominal lexicon}
\item{usageHistory}{list with actual usages of verbs, nouns, and verb and nominal markers}
\item{commonGround}{vector with lexemes recently discussed}
\item{collostructions}{list with collostruction frequencies for subject-verb, object-verb, index-referent, and noun marker-noun combinations}
\item{topic}{topic}
\item{wordOrder}{data frame with word order frequencies}
\item{topicPosition}{data frame with topic position frequencies}
}
\references{
Charles Yang (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{FOUND}
}
\examples{
adam=FIRSTSPEAKER()
str(adam)
}
\keyword{misc}
