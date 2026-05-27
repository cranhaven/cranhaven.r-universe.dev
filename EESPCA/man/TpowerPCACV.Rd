\name{tpowerPCACV}
\alias{tpowerPCACV}
\title{Sparsity parameter selection for the Yuan and Zhang TPower method using cross-validation.}
\description{
Sparsity parameter selection for PCA-based TPower using the cross-validation 
approach of Witten et al. as implemented by the \code{SPC.cv} method in the PMA package.
}
\usage{
    tpowerPCACV(X, k.values, nfolds=5)
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
\seealso{\code{\link{tpower}},\code{\link[PMA]{SPC.cv}}}
\references{
    \itemize{    
    \item Yuan, X.-T. and Zhang, T. (2013). Truncated power method for sparse eigenvalue problems. 
        J. Mach. Learn. Res., 14(1), 899-925.
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
    tpowerPCACV(X=X, k.values=k.values)
}