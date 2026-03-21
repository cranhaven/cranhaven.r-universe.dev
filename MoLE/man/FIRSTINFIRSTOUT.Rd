\name{FIRSTINFIRSTOUT}
\alias{FIRSTINFIRSTOUT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Order constituents by activation
}
\description{
If incremental production is assumed (cf. \code{world}), constituents are produced in order of activation.
}
\usage{
FIRSTINFIRSTOUT(speakerID, proposition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker who's formulating an utterance
}
  \item{proposition}{
The proposition to be uttered and whose constituents are reordered.
}
}
\value{
a proposition, i.e. a list:
\item{external}{representation of the external argument}
\item{internal}{representation of the internal argument, if identified}
\item{verb}{representation of the action argument}
\item{target}{target event to be described}
}
\references{
Balota, D. A. & Chumbley, J. I. (1985). The locus of word-frequency in the pronunciation task: Lexical access and/or production? Journal of memory and languages, 24, 89-106.

Bock, K., and Levelt, W.J.M. (1994). Language production. Grammatical encoding. IN M.A. Gernsbacher (Ed.). Handbook of psycholinguistics (pp.741-779). New York: Academic Press
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{PREPARE}
}
\examples{
FOUND()
situation=SITUATION(1)
(proposition=PROPOSITION(1, situation))
FIRSTINFIRSTOUT(speakerID, proposition)
}
\keyword{misc}
