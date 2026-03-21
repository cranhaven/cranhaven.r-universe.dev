\name{VMATCH}
\alias{VMATCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare vectors
}
\description{
Compare vectors properly taking into account uniform vectors (with same values on all dimensions) and non-specified dimensions.
}
\usage{
VMATCH(x, y, incomparable = 0, noise=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
target vector
}
  \item{y}{
vector (numeric or data frame/list) or set of vectors whose similarity to the target is to be determined
}
  \item{incomparable}{
Value to be returned for incomparable vectors, in which all dimension pairs contain underspecified values
}
  \item{noise}{
logical for addition of noise to outcome (default is TRUE)
}
}
\details{
Differences between vectors are determined per dimension, weighted (cf. \code{world$weigh}), and then averaged. If vectors are not specified for certain target dimensions, this does not count as a mismatch.
}
\value{
numeric
}
\author{
Sander Lestrade
}

\seealso{
\code{ACTOR}, \code{NOUNS}, \code{SITUATION}, \code{SELECTVERB}, \code{SELECTACTOR}, \code{SELECTUNDERGOER}, \code{PROPOSITION}, \code{REFCHECK}, \code{TOPICCOPY}, \code{GENERALIZE}, \code{CHECKSUCCESS}, \code{ANALYZE}, \code{TYPEMATCH}, \code{NOUNMORPHOLOGY}, \code{VERBMORPHOLOGY}, \code{INTERPRET.INT}, \code{SUCCESS}, \code{PERSONUPDATE}, \code{FUSE}, \code{SEMUPDATE}
}
\examples{
FOUND()
vectors=head(population[[1]]$nouns[,1:9])
target=vectors[1,]
vectors[2,]=NA
VMATCH(target, vectors)
}
\keyword{misc}
