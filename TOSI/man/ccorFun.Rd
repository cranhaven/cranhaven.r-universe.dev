\name{ccorFun}
\alias{ccorFun}
\title{Evaluate the smallest canonical correlation for two set of variables}
\description{
  Evaluate the smallest canonical correlation for two set of variables, each set of variables is represented by a matrix whose columns are variables.
}
\usage{
  ccorFun(hH, H)
}
\arguments{
  \item{hH}{a \code{n}-by-\code{q} matrix, one set of q variables.}
  \item{H}{a \code{n}-by-\code{q} matrix, the other set of q variables.}
}

\value{
  return a scalar value, the smallest canonical correlation.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{cancor}}.
}
\examples{
  dat <- gendata_Fac(n = 300, p = 500)
  res <- gsspFactorm(dat$X)
  ccorFun(res$hH, dat$H0)
}
