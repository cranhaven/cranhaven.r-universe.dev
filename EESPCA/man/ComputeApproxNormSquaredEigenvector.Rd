\name{computeApproxNormSquaredEigenvector}
\alias{computeApproxNormSquaredEigenvector}
\title{Approximates the normed squared eigenvector loadings}
\description{
Approximates the normed squared eigenvector loadings using a simplified version of the formula
associating normed squared eigenvector loadings with the eigenvalues of the full matrix and sub-matrices. 
}
\usage{
    computeApproxNormSquaredEigenvector(cov.X, v1, lambda1, max.iter=5, 
        lambda.diff.threshold=1e-6, trace=FALSE)
}
\arguments{
\item{cov.X}{Covariance matrix.}
\item{v1}{Principal eigenvector of \code{cov.X}, i.e, the loadings of the first PC.}
\item{lambda1}{Largest eigenvalue of \code{cov.X}.}
\item{max.iter}{Maximum number of iterations for power iteration method when computing sub-matrix eigenvalues.
    See description \code{\link{powerIteration}}.}
\item{lambda.diff.threshold}{Threshold for exiting the power iteration calculation. 
    See description \code{\link{powerIteration}}.}
\item{trace}{True if debugging messages should be displayed during execution.}
}
\value{
Vector of approximate normed squared eigenvector loadings.
}
\seealso{\code{\link{eespca}},\code{\link{powerIteration}}} 
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Estimate covariance matrix
    cov.X = cov(X)
    # Compute eigenvectors/values
    eigen.out = eigen(cov.X)
    v1 = eigen.out$vectors[,1]
    lambda1 = eigen.out$values[1]
    # Print true squared loadings
    v1^2
    # Compute approximate normed squared eigenvector loadings
    computeApproxNormSquaredEigenvector(cov.X=cov.X, v1=v1,
        lambda1=lambda1)  
}
