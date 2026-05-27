\name{reconstructionError}
\alias{reconstructionError}
\title{Calculates the reduced rank reconstruction error}
\description{
Utility function for computing the squared Frobenius norm of the residual matrix formed by 
subtracting from \code{X} a reduced rank approximation of matrix \code{X} generated from 
the top k principal components contained in matrix \code{V}.
}
\usage{
    reconstructionError(X,V,center=TRUE)
}
\arguments{
\item{X}{An n-by-p data matrix whose top k principal components are contained the p-by-k matrix \code{V}.}
\item{V}{A p-by-k matrix containing the loadings for the top k principal components of \code{X}.}
\item{center}{If true (the default), \code{X} will be mean-centered before the reconstruction
      error is computed. If the PCs in \code{V} were computed via SVD on 
      a mean-centered matrix or via eigen-decomposition of the sample covariance matrix, 
      this should be set to true.}
}
\value{
The squared Frobenius norm of the residual matrix.
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Perform PCA
    prcomp.out = prcomp(X)
    # Get rank 2 reconstruction error, which will be the minimum since the first 2 PCs are used
    reconstructionError(X, prcomp.out$rotation[,1:2])
    # Use all PCs to get approximately 0 reconstruction error
    reconstructionError(X, prcomp.out$rotation)    
}