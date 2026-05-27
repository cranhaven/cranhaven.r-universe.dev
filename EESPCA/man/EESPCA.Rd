\name{eespca}
\alias{eespca}
\title{Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA)}
\description{
Computes the first sparse principal component of the specified data matrix using 
the Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA) method.
}
\usage{
    eespca(X, max.iter=20, sparse.threshold, lambda.diff.threshold=1e-6, 
        compute.sparse.lambda=FALSE, sub.mat.max.iter=5, trace=FALSE)
}
\arguments{
\item{X}{An n-by-p data matrix for which the first sparse PC will be computed.}
\item{max.iter}{Maximum number of iterations for power iteration method. See \code{\link{powerIteration}}.}
\item{sparse.threshold}{Threshold on loadings used to induce sparsity. 
    Loadings below this value are set to 0. If not specified, defaults to \code{1/sqrt(p)}.}
\item{lambda.diff.threshold}{Threshold for exiting the power iteration calculation. 
    If the absolute relative difference in lambda is less than this threshold between subsequent iterations,
    the power iteration method is terminated. See \code{\link{powerIteration}}.} 
\item{compute.sparse.lambda}{If true, the sparse loadings will be used to compute the sparse eigenvalue.}
\item{sub.mat.max.iter}{Maximum iterations for computation of sub-matrix eigenvalues using
      the power iteration method. To maximize performance, set to 1. Uses the same lambda.diff.threshold.}
\item{trace}{True if debugging messages should be displayed during execution.}
}
\value{
A \code{list} with the following elements:
    \itemize{
      \item "v1": The first non-sparse PC as calculated via power iteration. 
      \item "lambda1": The variance of the first non-sparse PC as calculated via power iteration.
      \item "v1.sparse": First sparse PC.
      \item "lambda1.sparse": Variance of the first sparse PC. NA if compute.sparse.lambda is FALSE.
      \item "ratio": Vector of ratios of the sparse to non-sparse PC loadings.
     }
}
\seealso{\code{\link{eespcaForK}},\code{\link{computeApproxNormSquaredEigenvector}}, \code{\link{powerIteration}}} 
\references{
    \itemize{
    \item Frost, H. R. (2021). Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA). arXiv e-prints. https://arxiv.org/abs/2006.01924  
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Compute first sparse PC loadings using default threshold
    eespca(X=X)
}