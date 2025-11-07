\name{hcov}
\alias{hcov}
\title{
Hub covariance graph
}
\description{
Estimates a sparse covariance matrix with hub nodes using a Lasso penalty and a sparse group Lasso penalty.  The estimated covariance matrix Sigma can be decomposed as Sigma = Z + V + t(V).  The details are given in Section 4 in Tan et al. (2014).
}
\usage{
hcov(S, lambda1, lambda2=100000, lambda3=100000, convergence = 1e-10, 
maxiter = 1000, start = "cold", var.init = NULL,trace=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
A p by p correlation/covariance matrix.  Cannot contain missing values.       
}
  \item{lambda1}{
Non-negative regularization parameter for lasso on the matrix Z.  lambda=0 means no regularization.
}
  \item{lambda2}{
Non-negative regularization parameter for lasso on the matrix V.  lambda2=0 means no regularization.  The default value is lambda2=100000, encouraging V to be a zero matrix.
}
  \item{lambda3}{
Non-negative regularization parameter for group lasso on the matrix V.  lambda3=0 means no regularization.  The default value is lambda3=100000, encouraging V to be a zero matrix.
}
  \item{convergence}{
Threshold for convergence.  Devault value is 1e-10. 
}
  \item{maxiter}{
Maximum number of iterations of ADMM algorithm.  Default is 1000 iterations.
}
  \item{start}{
Type of start.  cold start is the default.  Using warm start, one can provide starting values for the parameters using object from hcov.
}
  \item{var.init}{
Object from hcov that provides starting values for all the parameters when start="warm" is specified.
}
  \item{trace}{
Default value of trace=FALSE.  If trace=TRUE, every 10 iterations of the ADMM algorithm is printed.}

}
\details{
This implements hub covariance graph estimation procedure using ADMM algorithm described in Section 4 in Tan et al. (2014), which estimates a sparse covariance matrix with hub nodes.  The estimated covariance matrix can be decomposed into Z + V + t(V): Z is a sparse matrix and V is a matrix that contains dense columns, each column corresponding to a hub node.  For the positive definite constraint Sigma >= epsilon*I that appears in the optimization problem, we choose epsilon to be 0.001 by default.  
   
The default value of lambda2=100000 and lambda3=100000 will yield  the estimator proposed by Xue et al. (2012).
   
Note that tuning parameters lambda1 determines the sparsity of the matrix Z, lambda2 determines the sparsity of the selected hub nodes, and lambda3 determines the selection of hub nodes.  



}
\value{
an object of class hcov.

Among some internal variables, this object includes the elements

\item{Sigma}{Sigma is the estimated covariance matrix. Note that Sigma = Z + V + t(V).}
\item{V}{V is the estimated matrix that contain hub nodes used to compute Sigma.}
\item{Z}{Z is the estimated sparse matrix used to compute Sigma.}
\item{objective}{Objective is the minimized objective value of the loss-function considered in Section 4 of Tan et al. (2014).}
\item{iteration}{The number of iterations of the ADMM algorithm until convergence.}
\item{hubind}{Indices for features that are estimated to be hub nodes}

}

\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research.  arXiv.org/pdf/1402.7349.pdf.

Xue et al. (2012).  Positive-definite l1-penalized estimation of large covariance matrices.  Journal of the American Staitstical Association, 107:1480-1491.
}
\author{
Kean Ming Tan
}

\seealso{
\code{\link{image.hcov}}
\code{\link{plot.hcov}}
\code{\link{summary.hcov}}
}
\examples{
#############################################
# Example for estimating covariance matrix
# with hubs
##############################################
library(mvtnorm)
set.seed(1)
n=100
p=100

# a covariance with 4 hubs

network <- HubNetwork(p,0.95,4,0.1,type="covariance")
Sigma <- network$Theta
hubind <- network$hubcol
x <- rmvnorm(n,rep(0,p),Sigma)
x <- scale(x)

# Estimate the covariance matrix
res1<-hcov(cov(x),0.3,0.2,1.2)
summary(res1)
# correctly identified two of the hub nodes

# Plot the matrices V and Z 
image(res1)
dev.off()
# Plot a graphical representation of the estimated covariance matrix --- covariance graph
plot(res1)

# Xue et al cannot identified any hub nodes
res2 <- hcov(cov(x),0.3)
summary(res2)
plot(res2)

}