\name{d2s}
\alias{d2s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dense to sparse conversion
}
\description{
Convert data from dense representation (matrix) to sparse representation (list of data frames).
}
\usage{
d2s(
  X,
  zero = 0,
  threshold = 1e-16,
  verbose= TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A \code{d x N} numeric matrix where \code{N} is the number of data points --- each column is an observation, and \code{d} is the dimensionality. Column-observation representation promotes cache locality.
}
  \item{zero}{
A numeric value. Elements in \code{X} satisfying \code{abs(X[i]} \code{-} \code{zero)} \code{<= threshold} are treated as zeros. Default 0.
}
  \item{threshold}{
A numeric value, explained above.
}
\item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}

\value{
A list of size \code{N}. \code{Value[[i]]} is a 2-column data frame. The 1st column is a sorted integer vector of the indexes of nonzero dimensions. Values in these dimensions are stored in the 2nd column as a numeric vector.
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
str(sparseX[1:5])
}






