\name{calcPCA}
\alias{calcPCA}

\title{Perform principal component analysis}

\description{
  Given a data matrix, the function allows to perform principal
  component analysis using a power method to get the eigendecomposition.
}

\usage{
  calcPCA(X, r, eta, itmax, err, normed, mySeed)
}

\arguments{
  \item{X}{Data matrix of size (m,n), m >= n.}
  \item{r}{Number of principal components, default: r=2.}
  \item{eta}{Power method tuning parameter, default: eta=10.}
  \item{itmax}{Maximum number of iteration in the power method,
       default: itmax=200.}
  \item{err}{Tolerance level in the power method, default: err=1e-8.}
  \item{normed}{TRUE (default) or FALSE for PCA using standardized
    data or not.}
  \item{mySeed}{An integer allowing to reproduce results from
    two different runs, default: mySeed=50.}
}

\details{
  X is usually a data matrix .
}

\value{This function returns a data frame containing 5 components
  \item{values}{Eigenvalues}
  \item{vectors}{Matrix with the eigenvectors.}
  \item{iter}{The number of iterations used in the eigendecomposition.}
  \item{li}{Projection of rows in the r principal components space.}
  \item{co}{Projection of columns in the r principal components space.}
}

\examples{
data(iris)
X <- as.matrix(iris[,1:4])
rownames(X) <- iris[,5]
res <- calcPCA(X, r=3)
res$values
pcol <- c(rep("cyan",50), rep("red",50), rep("blue",50))
plot(res$li[,1], res$li[,3], col = pcol)
}
