\name{DECOMPOSE}
\alias{DECOMPOSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Decompose words into morphemes
}
\description{
Decomposes words into morphemes on the basis of the lexical entries in the lexicon. If multiple decompositions are possible, all are returned.
}
\usage{
DECOMPOSE(hearerID, form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent
}
  \item{form}{
Word form that is considered for decomposition
}
}
\details{
Decomposition is not trivial: Because of sloppy pronunciation (\code{PRODUCE}) and differences between speakers, mental representations of morphemes need not match one-to-one the parts of an utterance.
Zero morphemes are not allowed. Reduced forms may become suffixes too. Suffixes must be minimally erosionMax long (should be automatically satisfied...). 
Function applies recursively (max twice)
}
\value{
A vector with morphologically analyzed words, in which morpheme-s are separate-d by hyphen-s ("-")
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ANALYZE}
}
\examples{
FOUND()
old=world$suffixThreshold
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
(utterance=PRODUCE(1, proposition))
(utterance=gsub(' ', '', utterance))
world$suffixThreshold=20
DECOMPOSE(2, utterance)
world$suffixThreshold=old
}
\keyword{misc}
