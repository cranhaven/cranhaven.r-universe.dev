\name{MAX}
\alias{MAX}
\alias{MIN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find maximum value
}
\description{
Extension of standard \code{max} and \code{min} functions with which rank position(s) can be specified and result can be either rank or value.
}
\usage{
MAX(vector, rank = 1, value = FALSE, rank.adjust = TRUE, forceChoice = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{
Vector in which maximum/minimum element needs to be identified
}
  \item{rank}{
value(s) or rank(s) of maximum values. 
}
  \item{value}{
Should value or rank be returned?
}
  \item{rank.adjust}{
If maximum value of range of ranks exceeds vector length, should this be adjusted?
}
  \item{forceChoice}{
In case of ties, should all results be returned or only one?
}
}
\value{
numeric vector (either value or rank)
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
If minimum value (of a range of) rank(s) exceeds vector length, results are meaningless.
}
\seealso{
\code{MIN}, \code{NOUNS}, \code{SITUATION}, \code{SELECTVERB}, \code{SELECTACTOR}, \code{SELECTUNDERGOER}, \code{REFCHECK}, \code{TOPICCOPY}, \code{GENERALIZE}, \code{CHECKSUCCESS}, \code{ANALYZE}, \code{TYPEMATCH}, \code{NOUNMORPHOLOGY}, \code{VERBMORPHOLOGY}, \code{INTERPRET.INT}, \code{INTERPRET}, \code{NOUNDESEMANTICIZATION}, \code{VERBDESEMANTICIZATION}, \code{SEMUPDATE}, \code{DIE}
}
\examples{
a=rep(1:10, 2)
MAX(a, rank=1:3, value=TRUE, forceChoice=TRUE)
MIN(a, rank=1:3, value=TRUE, forceChoice=TRUE)
}
\keyword{misc}
