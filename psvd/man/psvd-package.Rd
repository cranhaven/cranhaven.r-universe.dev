\name{psvd-package}
\docType{package}
\alias{psvd-package}

\title{Eigendecomposition, Singular-Values and the Power Method}

\description{
  The power method is used to compute simultaneously the
  eigenvectors of a square symmetric matrix. Using the classical method,
  all eigenvectors are computed. The method used here allows to compute the
  first r eigenvectors using only matrix multiplications and the Gram-Schmidt
  orthogonalization algorithm. The relationships between the eigendecomposition
  factors, on the one hand, and the PCA factors or SVD factors, on the order
  hand, are used to get SVD or PCA results).
}

\details{
  \tabular{ll}{
     Package: \tab psvd\cr
     Type: \tab Package\cr
     Version: \tab 0.1-0\cr
     Date: \tab 2024-10-02\cr
     License: \tab GPL (>= 2)\cr
  }

  Package psvd has the following functions:
  \tabular{rl}{
   calcSVD(): \tab Given a data matrix X of size (m,n), m >=n, this function
       allows to compute\cr \tab the singular value decomposition. \cr
   calcPCA(): \tab Given a data matrix X of size (m,n), m >=n, this function
       allows to\ compute\cr \tab the principal component analysis.\cr
   mGS(): \tab Modified Gramf-Schmidt orthogonalization method, R code,
          internal use.\cr
   mGSc(): \tab Modified Gramf-Schmidt orthogonalization method, C code,
           internal use.\cr
   eigenV(): \tab Computation of the eigenvectors matrix for a symmetric
       square matrix using\cr \tab the power method, R Code, internal use.\cr
   eigenVc(): \tab Computation of the eigenvectors matrix for a symmetric
       square matrix using\cr \tab the power method, C Code, internal use.
  }
}

\references{Dembele D. (2024), \emph{Manuscript in preparation}}

\author{Doulaye Dembele: \email{doulaye@igbmc.fr}}

\examples{
data(iris)
X <- as.matrix(iris[,1:4])
rownames(X) <- iris[,5]
res <- calcSVD(X, r=4)
res$d
res$v
res$iter
}
