\name{riflePCACV}
\alias{riflePCACV}
\title{Sparsity parameter selection via cross-validation for rifle method of Tan et al.}
\description{
Sparsity parameter selection for PCA-based rifle (as implemented by the
\code{rifle} method in the rifle package) using the cross-validation 
approach of Witten et al. as implemented by the \code{SPC.cv} method in the PMA package.
}
\usage{
    riflePCACV(X, k.values, nfolds=5)
}
\arguments{
\item{X}{n-by-p data matrix being evaluated via PCA.}
\item{k.values}{Set of truncation parameter values to evaluate via cross-validation. 
      Values must be between 1 and p.}
\item{nfolds}{Number of folds for cross-validation}    
}
\value{
k value that generated the smallest cross-validation error.
}
\seealso{\code{\link{rifleInit}}, \code{\link[rifle]{rifle}}, \code{\link[PMA]{SPC.cv}}}
\references{
    \itemize{    
    \item Tan, K. M., Wang, Z., Liu, H., and Zhang, T. (2018). 
        Sparse generalized eigenvalue problem: optimal statistical rates via truncated rayleigh flow. 
        Journal of the Royal Statistical Society: Series B (Statistical Methodology), 80(5), 1057-1086.
    \item Witten, D. M., Tibshirani, R., and Hastie, T. (2009). 
        A penalized matrix decomposition, with applications to sparse principal components and 
        canonical correlation analysis. Biostatistics, 10(3), 515-534.
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Generate range of k values to evaluate
    k.values = 1:5
    # Use 5-fold cross-validation to estimate optimal k value
    riflePCACV(X=X, k.values=k.values)
}
