\name{tpower}
\alias{tpower}
\title{Implementation of the Yuan and Zhang TPower method.}
\description{
Implements the TPower method by Yuan and Zhang. 
Specifically, it computes the sparse principal eigenvector using power iteration method 
where the estimated eigenvector is truncated on each iteration to retain just the
k eigenvector loadings with the largest absolute values with all other values set to 0.
}
\usage{
    tpower(X, k, v1.init, max.iter=10, lambda.diff.threshold=1e-6, trace=FALSE)
}
\arguments{
\item{X}{Matrix for which the largest eigenvector and eigenvalue will be computed.}
\item{k}{Must be an integer between 1 and ncol(X). The estimated eigenvector is truncated on each iteration to retain
    only the k loadings with the largest absolute values, all other loadings are set to 0.} 
\item{v1.init}{If specified, the power iteration calculation will be initialized using this vector, otherwise,
    the calculation will be initialized using a unit vector with equal values.}
\item{max.iter}{Maximum number of iterations for power iteration method.}
\item{lambda.diff.threshold}{Threshold for exiting the power iteration calculation. 
    If the absolute relative difference in computed eigenvalues is less than this threshold between subsequent iterations,
    the power iteration method is terminated.}
\item{trace}{True if debugging messages should be displayed during execution.}
}
\value{The estimated sparse principal eigenvector.}
\seealso{\code{\link{powerIteration}},\code{\link{tpowerPCACV}}}
\references{
    \itemize{    
    \item Yuan, X.-T. and Zhang, T. (2013). Truncated power method for sparse eigenvalue problems. J. Mach. Learn. Res., 14(1), 899-925.    
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Compute first sparse PC loadings with 2 non-zero elements
    tpower(X=cov(X), k=2)
}