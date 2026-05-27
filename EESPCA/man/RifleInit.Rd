\name{rifleInit}
\alias{rifleInit}
\title{Computes the initial eigenvector for the rifle method of Tan et al.}
\description{
Computes the initial eigenvector for the rifle method of Tan et al. (as implemented by the
\code{rifle} method in the rifle R package) using the \code{initial.convex} method 
from the rifle package with lambda=sqrt(log(p)/n) and K=1.
}
\usage{
    rifleInit(X)
}
\arguments{
\item{X}{n-by-p data matrix to be evaluated via PCA.}
}
\value{
Initial eigenvector to use with rifle method.
}
\seealso{\code{\link{riflePCACV}}, \code{\link[rifle]{rifle}}, \code{\link[rifle]{initial.convex}}}
\references{
    \itemize{    
    \item Tan, K. M., Wang, Z., Liu, H., and Zhang, T. (2018). 
        Sparse generalized eigenvalue problem: optimal statistical rates via truncated rayleigh flow. 
        Journal of the Royal Statistical Society: Series B (Statistical Methodology), 80(5), 1057-1086.
    }
}
\examples{
    set.seed(1)
    # Simulate 10x5 MVN data matrix
    X=matrix(rnorm(50), nrow=10)
    # Compute initial eigenvector to use with rifle method
    v1.init = rifleInit(X)
    # Use with rifle method to get first PC loadings with 2 non-zero elements
    rifle(A=cov(X), B=diag(5), init=v1.init, k=2)
}