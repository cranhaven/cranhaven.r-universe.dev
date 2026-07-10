\name{gendata_Fac}
\alias{gendata_Fac}
\title{Generate simulated data}
\description{
  Generate simulated data from high dimensional sparse factor model.
}
\usage{
  gendata_Fac(n, p, seed=1, q=6, pzero= floor(p/4),
              sigma2=0.1, gamma=1, heter=FALSE, rho=1)

}
\arguments{
  \item{n}{a positive integer, the sample size.}
  \item{p}{an positive integer, the variable dimension.}
  \item{seed}{a nonnegative integer, the random seed, default as 1.}
  \item{q}{a positive integer, the number of factors.}
  \item{pzero}{a positive integer, the number of zero loading vectors, default as p/4.}
  \item{sigma2}{a positive real number, the homogenous variance of error term.}
  \item{gamma}{a positive number, the common component of heteroscedasticity of error term.}
  \item{heter}{a logical value, indicates whether generate heteroscendastic error term.}
  \item{rho}{a positive number, controlling the magnitude of loading matrix.}
}

\value{
  return a list including two components:

  \item{X}{a \code{n}-by-\code{p} matrix, the observed data matrix.}
  \item{H0}{a \code{n}-by-\code{q} matrix, the true lantent factor matrix.}
  \item{B0}{a \code{p}-by-\code{q} matrix, the true loading matrix, the last pzero rows are vectors of zeros.}
  \item{ind_nz}{a integer vector, the index vector for which rows of \code{B0} not zeros.}
}

\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{Factorm}}.
}
\examples{
  dat <- gendata_Fac(n=300, p = 500)
  str(dat)
}
\keyword{Factor}
\keyword{Feature}
