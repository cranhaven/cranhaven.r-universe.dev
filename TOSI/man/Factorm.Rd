\name{Factorm}
\alias{Factorm}
\title{Factor Analysis Model}
\description{
  Factor analysis to extract latent linear factor and estimate loadings.
}
\usage{
  Factorm(X, q=NULL)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{q}{an integer between 1 and \code{p} or \code{NULL}, default as \code{NULL} and automatically choose q by the eigenvalue ratio method.}
}

\value{
  return a list with class named \code{fac}, including following components:

  \item{hH}{a \code{n}-by-\code{q} matrix, the extracted lantent factor matrix.}
  \item{hB}{a \code{p}-by-\code{q} matrix, the estimated loading matrix.}
  \item{q}{an integer between 1 and \code{p}, the number of factor extracted.}
  \item{sigma2vec}{a p-dimensional vector, the estimated variance for each error term in model.}
  \item{propvar}{a positive number between 0 and 1, the explained propotion of cummulative variance by the \code{q} factors.}
  \item{egvalues}{a n-dimensional(n<=p) or p-dimensional(p<n) vector, the eigenvalues of sample covariance matrix.}
}
\references{Fan, J., Xue, L., and Yao, J. (2017). Sufficient forecasting using factor models. Journal of Econometrics.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{factor}}.
}
\examples{
  dat <- gendata_Fac(n = 300, p = 500)
  res <- Factorm(dat$X)
  ccorFun(res$hH, dat$H0) # the smallest canonical correlation
}
\keyword{Factor}
\keyword{Feature}
