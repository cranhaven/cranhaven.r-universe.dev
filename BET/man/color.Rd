\name{get.signs}
\alias{get.signs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Signs of Colors of all Points for all Interactions
}
\description{
\code{get.signs} returns all the signs of colors for each point under all interactions up to depth \code{d} in marginal binary expansions for the tests \code{BET} and \code{BETs}.
}
\usage{
get.signs(X, dep, unif.margin = FALSE)
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
The result is a dataframe with \eqn{n} rows and \eqn{2^(p*dep)} columns, where \eqn{p} is the number of columns of \code{X} and \eqn{n} is the number of rows of \code{X}. The values of \eqn{1} or \eqn{-1} stand for the sign of color, while the marginal interactions return \eqn{0}.
}

\examples{
v <- runif(128, -pi, pi)
X1 <- cos(v) + 2.5 * rnorm(128, 0, 1/20)
X2 <- sin(v) + 2.5 * rnorm(128, 0, 1/20)
get.signs(cbind(X1, X2), 3)
}
