\name{block.fct}
\alias{block.fct}
\title{
Matrix Direct Sum
}
\description{
Matrix direct sum function. Creates a block diagonal matrix.
}
\usage{
block.fct(...)
}
\arguments{
  \item{...}{\R matrices (matrix).}
}
\value{
\code{block.fct} returns a block diagonal matrix, where the direct sum is
in the order of the input matrices (matrix).
}
\author{
Joseph B. Lang
}
\examples{
A <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
B <- matrix(c(5, 6, 7, 8, 9, 10), nrow = 2, byrow = TRUE)
C <- matrix(c(11, 12, 13, 14), nrow = 1, byrow = TRUE)
block.fct(A, B, C)
}
\keyword{array}% use one of  RShowDoc("KEYWORDS")
\keyword{algebra}% __ONLY ONE__ keyword per line
