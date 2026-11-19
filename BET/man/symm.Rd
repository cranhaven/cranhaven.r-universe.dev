\name{symm}
\alias{symm}
\title{
Symmetry Statistics for all Interactions
}
\description{
\code{symm} returns all the symmetry statistics up to depth \code{d} in marginal binary expansions for the tests \code{BET} and \code{BETs}.
}
\usage{
symm(
  X,
  dep,
  unif.margin = FALSE,
  print.sample.size = TRUE
)
}
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
  \item{print.sample.size}{
  logicals. If \code{TRUE} print the sample size. Default to be \code{TRUE}.
  }
}

\value{
The result is a dataframe with \eqn{(p+2)} columns, where \eqn{p} is the number of columns of \code{X}. The first column gives the binary index for all variables, the next \eqn{p} columns displays all the interactions of respective variables, the last column of \code{Statistics} gives the respective symmetry statistic.
}

\examples{
v <- runif(128, -pi, pi)
X1 <- cos(v) + 2.5 * rnorm(128, 0, 1/20)
X2 <- sin(v) + 2.5 * rnorm(128, 0, 1/20)
symm(cbind(X1, X2), 3)
}


