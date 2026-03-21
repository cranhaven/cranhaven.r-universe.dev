\name{CANDIDATESCORE}
\alias{CANDIDATESCORE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Score candidate expressions
}
\description{
Provides each candidate expression for some meaning or function with a score in which (depending on the model settings) semantic match, lexeme activation, (relative) frequency of use, recency, collostruction frequency, semantic weight, and/or economy of expression are taken into consideration.
}
\usage{
CANDIDATESCORE(lexicon, type = "referringExpression")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lexicon}{
lexicon with candidate expresions
}
  \item{type}{
Type of function for which an expression has to be found (\code{referringExpression}, \code{nounMarker}, \code{verbMarker}, or \code{pronoun}).
}
}
\details{
Collostruction frequencies are determined differently for different type of functions.
The lighter, the better; recency starts with 0.
}
\value{
Vector of scores, corresponding to the entries evaluated.
}
\author{
Sander Lestrade
}
\note{
Match and collostruction frequency are calculated separately before \code{CANDIDATESCORE} can apply. In the example below, the latter is randomly set for illustration purposes.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{SELECTVERB}, \code{SELECTACTOR}, \code{SELECTUNDERGOER}, \code{REFCHECK}, \code{TOPICCOPY}, \code{GENERALIZE}, \code{CHECKSUCCESS}
}
\examples{
FOUND()
lexicon=head(population[[1]]$nouns)
lexicon$match=VMATCH(lexicon[1,1:9], lexicon)
lexicon$collostruction=sample(100, nrow(lexicon))	
lexicon$score=CANDIDATESCORE(lexicon)
}
\keyword{misc}
