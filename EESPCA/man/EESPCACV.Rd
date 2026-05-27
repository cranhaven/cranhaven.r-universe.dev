\name{eespcaCV}
\alias{eespcaCV}
\title{Cross-validation for Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA)}
\description{
Performs cross-validation of EESPCA to determine the optimal 
sparsity threshold. Selection is based on the minimization of reconstruction error.
Based on the cross-validation approach of Witten et al. as implemented by the \code{SPC.cv} method in the PMA package.
}
\usage{
    eespcaCV(X, max.iter=20, sparse.threshold.values, nfolds=5, 
        lambda.diff.threshold=1e-6, compute.sparse.lambda=FALSE, 
        sub.mat.max.iter=5, trace=FALSE)
}
\arguments{
\item{X}{See description for \code{\link{eespca}}}
\item{max.iter}{See description for \code{\link{eespca}}}
\item{sparse.threshold.values}{Vector of threshold values to evaluate via cross-validation. 
    See description for \code{\link{eespca}} for details.}
\item{nfolds}{Number of cross-validation folds.}
\item{lambda.diff.threshold}{See description for \code{\link{eespca}}}
\item{compute.sparse.lambda}{See description for \code{\link{eespca}}}
\item{sub.mat.max.iter}{See description for \code{\link{eespca}}}
\item{trace}{See description for \code{\link{eespca}}}
}
\value{
A \code{list} with the following elements:
    \itemize{
      \item "cv": The mean of the out-of-sample reconstruction error computed for each threshold.
      \item "cv.error": The standard deviations of the means of the out-of-sample reconstruction error computed for each
        threshold.
      \item "best.sparsity": Threshold value with the lowest mean reconstruction error.
      \item "best.sparsity.1se": Threshold value whose mean reconstruction error is within 1 standard error of the lowest.      
      \item "nonzerovs": Mean number of nonzero values for each threshold.
      \item "sparse.threshold.values": Tested threshold values.
      \item "nfolds": Number of cross-validation folds.
     }
}
\seealso{\code{\link{eespca}}, \code{\link[PMA]{SPC.cv}}} 
\references{
    \itemize{
    \item Frost, H. R. (2021). Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA). arXiv e-prints. https://arxiv.org/abs/2006.01924  
    \item Witten, D. M., Tibshirani, R., and Hastie, T. (2009). 
        A penalized matrix decomposition, with applications to sparse principal components and 
        canonical correlation analysis. Biostatistics, 10(3), 515-534.            
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Generate range of threshold values to evaluate
    default.threshold = 1/sqrt(5)
    threshold.values = seq(from=.5*default.threshold, to=1.5*default.threshold, length.out=10)
    # Use 5-fold cross-validation to estimate optimal sparsity threshold
    eespcaCV(X=X, sparse.threshold.values=threshold.values)
}
