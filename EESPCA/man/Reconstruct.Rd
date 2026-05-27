\name{reconstruct}
\alias{reconstruct}
\title{Calculates the reduced rank reconstruction}
\description{
Utility function for computing the reduced rank reconstruction of \code{X} using the PC 
loadings in \code{V}.
}
\usage{
    reconstruct(X,V,center=TRUE)
}
\arguments{
\item{X}{An n-by-p data matrix whose top k principal components are contained the p-by-k matrix \code{V}.}
\item{V}{A p-by-k matrix containing the loadings for the top k principal components of \code{X}.}
\item{center}{If true (the default), \code{X} will be mean-centered before the reconstruction
      is computed. If the PCs in \code{V} were computed via SVD on 
      a mean-centered matrix or via eigen-decomposition of the sample covariance matrix, 
      this should be set to true.}
}
\value{
Reduced rank reconstruction of X.
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Perform PCA
    prcomp.out = prcomp(X)
    # Get rank 2 reconstruction
    reconstruct(X, prcomp.out$rotation[,1:2])
}