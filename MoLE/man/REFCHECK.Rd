\name{REFCHECK}
\alias{REFCHECK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check referential capacity
}
\description{
Check if pronoun is sufficiently strong to establish reference to referent. If not, another word is recruited for support, the pronoun being suffixed to the verb.
}
\usage{
REFCHECK(speakerID, proposition, situation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{proposition}{
Proposition in which the referential expressions are checked
}
  \item{situation}{
Situation in which referential relations have to be established
}
}
\details{
Strength is determined by formal mass, i.e. simple word length.
Non-local arguments are matched with real-world argument; local pronoun with role, after which marker is removed. If there's no local pronominal paradigm yet, select prominent noun for local ref
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument, checked for strength}
\item{internal}{representation of the internal argument, if identified, checked for strength}
\item{verb}{representation of the action argument, possibly including person indexing if original expression for (one of the) event participants fell short}
\item{target}{target event to be described}
}
\references{
Zeevat, Henk. 2007. "Simulating recruitment in evolution". Cognitive Foundations of Interpretation ed. by G. Bouma, I. Kraemer & J. Zwarts, 175-194. Amsterdam: Royal Netherlands Academy of Arts and Sciences.

Ariel, M. (1999). The development of person agreement markers: From pronouns to higher accessibility markers. In M. Barlow & S. Kemmer (Eds.), Usage based models of language (p. 197-260). Stanford: CSLI.
}
\author{
Sander Lestrade
}

\seealso{
\code{PREPARE}
}
\examples{
FOUND()
situation=SITUATION(1)
(proposition=PROPOSITION(1, situation))
REFCHECK(1, proposition, situation)	
#only effective if words have grammaticalized already
}
\keyword{misc}
