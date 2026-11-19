\name{cell.counts}
\alias{cell.counts}

\title{
Counts the amount of points in each cell after binary expansion.
}
\description{
\code{cell.counts} returns the amount of data points in each cell getting from binary expansion.
}
\usage{
cell.counts(X, dep, unif.margin = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
a matrix to be tested.
}
  \item{dep}{
depth of the marginal binary expansions.
}
  \item{unif.margin}{
  logicals. If \code{TRUE} the data has been uniformed based on empirical cumulative distribution function. Default to be \code{FALSE} and the function uniforms the data.
  }
}

\value{
The result is a dataframe with 2 rows and \eqn{2^(p*dep)} columns, where \eqn{p} is the number of columns of \code{X}. The first column is the binary index, the second column is the amount of data points.
}

\examples{
v <- runif(128, -pi, pi)
X1 <- cos(v) + 2.5 * rnorm(128, 0, 1/20)
X2 <- sin(v) + 2.5 * rnorm(128, 0, 1/20)
cell.counts(cbind(X1, X2), 3)
}
