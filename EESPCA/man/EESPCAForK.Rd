\name{eespcaForK}
\alias{eespcaForK}
\title{Multi-PC version of Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA)}
\description{
Computes multiple sparse principal components of the specified data matrix via sequential application of
the Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA) algorithm.
After computing the first sparse PC via the \code{\link{eespca}} function, 
subsequent sparse PCs are computing by repeatedly applying \code{\link{eespca}} to the residual matrix formed
by subtracting the reconstruction of \code{X} from the original \code{X}. 
Multiple sparse PCs are not guaranteed to be orthogonal. 

Note that the accuracy of the sparse approximation declines substantially for PCs with very small
variances. To avoid this issue, \code{k} should not be set higher than the number of statistically 
significant PCs according to a Tracey-Widom test.
}
\usage{
    eespcaForK(X, k=2, max.iter=20, sparse.threshold, lambda.diff.threshold=1e-6, 
        compute.sparse.lambda=FALSE, sub.mat.max.iter=5, trace=FALSE)
}
\arguments{
\item{X}{An n-by-p data matrix for which the first \code{k} sparse PCs will be computed.}
\item{k}{The number of sparse PCs to compute. The specified k must be 2 or greater (for k=1, use
    the \code{\link{eespca}} method). A check is made that k is not greater than the maximum theoretical
    rank of X but, for performance reasons, a check is NOT made that
    k is less than or equal to the actual rank of X. }
\item{max.iter}{See description for \code{\link{eespca}}}
\item{sparse.threshold}{See description for \code{\link{eespca}}}
\item{lambda.diff.threshold}{See description for \code{\link{eespca}}}
\item{compute.sparse.lambda}{See description for \code{\link{eespca}}}
\item{sub.mat.max.iter}{See description for \code{\link{eespca}}}
\item{trace}{See description for \code{\link{eespca}}}
}
\value{
A \code{list} with the following elements:
    \itemize{
     \item "V": Matrix of sparse loadings for the first k PCs. 
     \item "lambdas": Vector of variances of the first k sparse PCs.
     }
}
\seealso{\code{\link{eespca}}}
\references{
    \itemize{
    \item Frost, H. R. (2021). Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA). arXiv e-prints. https://arxiv.org/abs/2006.01924  
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Get first two sparse PCs
    eespcaForK(X=X, sparse.threshold=1/sqrt(5), k=2)
}