\name{calcSVD}
\alias{calcSVD}

\title{Perform singular values decomposition}

\description{
  Given a data matrix, the function allows to perform a singular
  decomposition using a power method and relationship between SVD factors
  and the eigendecomposition factors.
}

\usage{
  calcSVD(X, r, eta, q, itmax, err, mySeed)
}

\arguments{
  \item{X}{Data matrix of size (m,n), m >= n.}
  \item{r}{Rank r approximation, default: r=2.}
  \item{eta}{Power method tuning parameter, default: eta=10.}
  \item{q}{Power method tuning parameter, default: q=2.}
  \item{itmax}{Maximum number of iteration in the power method,
       default: itmax=200.}
  \item{err}{Tolerance level in the power method, default: err=1e-8.}
  \item{mySeed}{An integer allowing to reproduce results from
    two different runs, default: mySeed=50.}
}

\details{ X is usually a data matrix.}

\value{
  This function returns a data frame containing 4 components
  \item{d}{Singular values.}
  \item{u}{Matrix with the right eigenvectors.}
  \item{v}{Matrix with the right eigenvectors.}
  \item{iter}{The number of iterations used in the eigendecomposition.}
}

\examples{
data(iris)
X <- as.matrix(iris[,1:4])
rownames(X) <- iris[,5]
res <- calcSVD(X, r=3)
res$d
res$v
res$iter
}
