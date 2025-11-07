\name{hglasso}
\alias{hglasso}

\title{
Hub graphical lasso}
\description{
Estimates a sparse inverse covariance matrix with hub nodes using a Lasso penalty and a sparse group Lasso penalty.  The estimated inverse covariance matrix Theta can be decomposed as Theta = Z + V + t(V), where Z is a sparse matrix and V is a matrix that contains hub nodes.  The details are given in Tan et al. (2014).
}
\usage{
hglasso(S, lambda1, lambda2=100000, lambda3=100000, convergence = 1e-10
, maxiter = 1000, start = "cold", var.init = NULL, trace=FALSE)
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
Type of start.  cold start is the default.  Using warm start, one can provide starting values for the parameters using object from hglasso.
}
  \item{var.init}{
Object from hglasso that provides starting values for all the parameters when start="warm" is specified.
}

  \item{trace}{
Default value of trace=FALSE.  If trace=TRUE, every 10 iterations of the ADMM algorithm is printed.}

}
\details{
This implements hub graphical lasso using ADMM Algorithm (see Algorithm 1) described in Tan et al. (2014), which estimates a sparse inverse covariance matrix with hub nodes.  The estimated inverse covariance matrix can be decomposed into Z + V + t(V): Z is a sparse matrix and V is a matrix that contains dense columns, each column corresponding to a hub node.  

The default value of lambda2=100000 and lambda3=100000 will yield the graphical lasso estimate as in Friedman et al. (2007) 'Sparse inverse covariance estimation with lasso'.   

Note that tuning parameters lambda1 determines the sparsity of the matrix Z, lambda2 determines the sparsity of the selected hub nodes, and lambda3 determines the selection of hub nodes.  

This algorithm uses a block diagonal screening rule to speed up computations considerably.  Details are given in Theorem 1 in Tan et al. (2014) 'Learning graphical models with hubs'.  The idea is as follow: we first check whether the solution to the hglasso problem will be block diagonal, for a given set of tuning parameters, using Theorem 1.  If so, then one can simply apply hglasso to each block separately, leading to massive speed improvements.  Similar idea has been used to obtain a sparse inverse covariance matrix as in Friedman et al. (2007) in the glasso package.
}
\value{
an object of class hglasso.  

Amog some internal variables, this object include the elements 

\item{Theta}{Theta is the estimated inverse covariance matrix. Note that Theta = Z + V + t(V).}
\item{V}{V is the estimated matrix that contains hub nodes used to compute Theta.}
\item{Z}{Z is the estimated sparse matrix used to compute Theta.}
\item{objective}{Objective is the minimized objective value of the loss-function considered in Section 3 of Tan et al. (2014).}
\item{hubind}{Indices for features that are estimated to be hub nodes}
%\item{blocklist}{A list containing indices of features within each block.}
%\item{unconnected}{Indices for features that are unconnected to every other features.}

}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.

Friedman et al. (2007). Sparse inverse covariance estimation with the lasso. Biostatistics, 9(3):432-441.

Witten et al. (2011). New insights and faster computations for the graphical lasso.  Journal of Computational and Graphical Statistics, 20(4):892-900.

}
\author{
Kean Ming Tan
}



\seealso{
  \code{\link{image.hglasso}}
  \code{\link{plot.hglasso}}
  \code{\link{summary.hglasso}}
  \code{\link{hglassoBIC}}
  }
\examples{
##############################################
# Example from Figure 1 in the manuscript
# A toy example to illustrate the results from 
# Hub Graphical Lasso
##############################################
library(mvtnorm)
library(glasso)
set.seed(1)
n=100
p=100

# A network with 4 hubs
network<-HubNetwork(p,0.99,4,0.1)
Theta <- network$Theta
truehub <- network$hubcol
# The four hub nodes have indices 14, 42, 45, 78
print(truehub)

# Generate data matrix x
x <- rmvnorm(n,rep(0,p),solve(Theta))
x <- scale(x)

# Run Hub Graphical Lasso to estimate the inverse covariance matrix
res1 <- hglasso(cov(x),0.3,0.3,1.5)

# print out a summary of the object hglasso
summary(res1)
# we see that the estimated hub nodes have indices 14, 42, 45, 78
# We successfully recover the 4 hub nodes

# Run hglasso using with and without warm start.  
# system.time(hglasso(cov(x),0.31,0.3,1.5))

# system.time(hglasso(cov(x),0.31,0.3,1.5,start="warm",var.init=res1))

# Run hglasso with larger lambda2, encouraging the hub nodes to be more sparse
res2 <- hglasso(cov(x),0.3,0.35,1.5)

# Run hglasso with lambda2=lambda3=100000, the solution is the 
# same as the graphical lasso solution obtain from glasso package
res3 <- hglasso(cov(x),0.3)
res4 <- glasso(cov(x),0.3,penalize.diagonal=FALSE)
# print the frobenius norm of the difference between the two estimates
print(sum((res3$Theta-res4$wi)^2))

}

