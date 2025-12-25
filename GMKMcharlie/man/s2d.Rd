\name{s2d}
\alias{s2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sparse to dense conversion
}
\description{
Convert data from sparse representation (list of data frames) to dese representation (matrix).
}
\usage{
s2d(
  X,
  d,
  zero = 0,
  verbose = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A list of size \code{N}, the number of observations. \code{X[[i]]} is a 2-column data frame. The 1st column is a sorted integer vector of the indexes of nonzero dimensions. Values in these dimensions are stored in the 2nd column as a numeric vector.
}
  \item{d}{
An integer. The dimensionality of \code{X}. \code{d} MUST be no less than the maximum of all index vectors in \code{X}.
}
  \item{zero}{
A numeric value. In the result matrix, entries not registered in \code{X} will be filled with \code{zero}.
}
\item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}

\value{
A \code{d x N} numeric matrix.
}

\examples{
N = 2000L
d = 3000L
X = matrix(rnorm(N * d) + 2, nrow = d)
# Fill many zeros in X:
X = apply(X, 2, function(x) {
  x[sort(sample(d, d * runif(1, 0.95, 0.99)))] = 0; x})
# Get the sparse version of X.
sparseX = GMKMcharlie::d2s(X)
# Convert it back to dense.
X2 = GMKMcharlie::s2d(sparseX, d)
range(X - X2)
}









