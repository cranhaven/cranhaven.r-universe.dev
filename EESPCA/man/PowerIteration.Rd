\name{powerIteration}
\alias{powerIteration}
\title{Power iteration method for calculating principal eigenvector and eigenvalue.}
\description{
Computes the principal eigenvector and eigenvalue of the specified matrix using the power iteration method.
Includes support for truncating the estimated eigenvector on each iteration to retain just the
k eigenvector loadings with the largest absolute values with all other values set to 0,
i.e., the the TPower method by Yuan & Zhang.  
}
\usage{
    powerIteration(X, k, v1.init, max.iter=10, lambda.diff.threshold=1e-6, trace=FALSE)
}
\arguments{
\item{X}{Matrix for which the largest eigenvector and eigenvalue will be computed.}
\item{k}{If specified, the estimated eigenvector is truncated on each iteration to retain
    only the k loadings with the largest absolute values, all other loadings are set to 0. 
    Must be an integer between 1 and ncol(X).} 
\item{v1.init}{If specified, the power iteration calculation will be initialized using this vector, otherwise,
    the calculation will be initialized using a unit vector with equal values.}
\item{max.iter}{Maximum number of iterations for power iteration method.}
\item{lambda.diff.threshold}{Threshold for exiting the power iteration calculation. 
    If the absolute relative difference in computed eigenvalue is less than this threshold between subsequent iterations,
    the power iteration method is terminated.}
\item{trace}{True if debugging messages should be displayed during execution.}
}
\value{
A \code{list} with the following elements:
    \itemize{
      \item "v1": The principal eigenvector of \code{X}.
      \item "lambda": The largest eigenvalue of \code{X}.
      \item "num.iter": Number of iterations of the power iteration method before termination.
     }
}
\seealso{\code{\link{eespca}}}
\references{
    \itemize{    
    \item Yuan, X.-T. and Zhang, T. (2013). Truncated power method for sparse eigenvalue problems. 
        J. Mach. Learn. Res., 14(1), 899-925.
     }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Compute sample covariance matrix
    cov.X = cov(X)
    # Use power iteration to get first PC loadings using default initial vector
    powerIteration(X=cov.X)
}
 