\name{ANALYZE}
\alias{ANALYZE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine sentence constituents
}
\description{
Decomposes an utterance into its constituents and their parts (e.g. verb and/or noun markers). All possible analyses are tried, the best is selected.
}
\usage{
ANALYZE(hearerID, utterance, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent in the population
}
  \item{utterance}{
The utterance to be analyzed.
}
  \item{situation}{
The situation in which the utterance is uttered.
}
}
\details{
Situation argument is necessary to determine which referential expressions have most likely been used.
Suffix could be incorporated noun (l. 51-70), the rest could be a verb, a noun, a verb adposition, or noun adposition (l. 71-102)
Nouns can have single suffix only (change once number is implemented; l. 92)
Default interpretation is noun (cf. Heine & Kuteva)
Analysis starts with identifying verb
If verb can not be found by lexeme match, verb suffixes are used, if still unclear plausibility of alternative analysis is checked. 
Combinations of nouns with local person markers are penalized (to be removed if possessive marking is modelled), just like combinations of nouns with multiple markers (to be removed if case stacking is allowed)

}
\value{
A data frame with the identified constituents and their analyses as entries.
}
\references{
Heine, Bernd & Tania Kuteva (2007), The genesis of grammar. A reconstruction. Oxford: Oxford University Press.
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
(utterance=PRODUCE(1, proposition))
ANALYZE(2, utterance, situation)
}
\keyword{misc}
