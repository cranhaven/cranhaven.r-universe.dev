\name{hbn}
\alias{hbn}

\title{
Hub binary network}
\description{
Estimates a binary network with hub nodes using a Lasso penalty and a sparse group Lasso penalty.  The estimated Theta matrix can be decomposed as Theta = Z + V + t(V), where Z is a sparse matrix and V is a matrix that contains hub nodes.  The details are given in Tan et al. (2014).
}
\usage{
hbn(X, lambda1, lambda2=100000, lambda3=100000, convergence = 1e-8
, maxiter = 1000, start = "cold", var.init = NULL, trace=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
An n by p data matrix.  Cannot contain missing values.       
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
Threshold for convergence.  Devault value is 1e-8. 
}
  \item{maxiter}{
Maximum number of iterations of ADMM algorithm.  Default is 1000 iterations.
}
  \item{start}{
Type of start.  cold start is the default.  Using warm start, one can provide starting values for the parameters using object from hbn.
}
  \item{var.init}{
Object from hbn that provides starting values for all the parameters when start="warm" is specified.
}

  \item{trace}{
Default value of trace=FALSE.  If trace=TRUE, every 10 iterations of the ADMM algorithm is printed.}

}
\details{
This implements hub binary network using ADMM algorithm (see Algorithm 1 and Section 5) in Tan et al. (2014).  The estimated Theta matrix can be decomposed into Z + V + t(V): Z is a sparse matrix and V is a matrix that contains dense columns, each column corresponding to a hub node.  

The default value of lambda2=100000 and lambda3=100000 will yield the sparse binary network model estimate as in Hofling and Tibshirani  (2009) 'Estimation of sparse binary pairwise Markov networks using pseudo-likelihoods'.   

The tuning parameters lambda1 determines the sparsity of the matrix Z, lambda2 determines the sparsity of the selected hub nodes, and lambda3 determines the selection of hub nodes.  

Within each iteration of the ADMM algorithm, we need to perform an iterative procedure to obtain an update for the matrix Theta since there is no closed form solution for Theta.  The Barzilai-Borwein method is used for this purpose (Barzilai and Borwein, 1988).  For details, see Algorithm 2 in Appendix F in Tan et al. (2014).    

Note: we recommend using this function for moderate size network. For instance, network with 50-100 variables.  

}
\value{
an object of class hbn.  

Among some internal variables, this object includes the elements 

\item{Theta}{Theta is the estimated inverse covariance matrix. Note that Theta = Z + V + t(V).}
\item{V}{V is the estimated matrix that contains hub nodes used to compute Theta.}
\item{Z}{Z is the estimated sparse matrix used to compute Theta.}
\item{hubind}{Indices for features that are estimated to be hub nodes}
}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.

Hofling, H. and Tibshirani, R. (2009). Estimation of sparse binary pairwise Markov networks using pseudo-likelihoods. Journal of Machine Learning Research, 10:883-906. 

Barzilai, J. and Borwein, J. (1988).  Two-point step size gradient methods. IMA Journal of Numerical Analysis, 8:141-148.
}
\author{
Kean Ming Tan and Karthik Mohan
}



\seealso{
  \code{\link{image.hglasso}}
  \code{\link{plot.hglasso}}
  \code{\link{summary.hglasso}}
  \code{\link{binaryMCMC}}
  }
\examples{
##############################################
# An implementation of Hub Binary Network
##############################################
#set.seed(1000)
#n=50
#p=5

# A network with 2 hubs
#network<-HubNetwork(p,0.95,2,0.1,type="binary")
#Theta <- network$Theta
#truehub <- network$hubcol
# The four hub nodes have indices 4,5
#print(truehub)

# Generate data matrix x
#X <- binaryMCMC(n,Theta,burnin=500,skip=100)

# Run Hub Binary Network to estimate Theta
#res1 <- hbn(X,2,1,3,trace=TRUE)

# print out a summary of the object hbn
#summary(res1)

# We see that the estimated hub nodes have indices 1,5
# We successfully recover the hub nodes

# Plot the resulting network
# plot(res1) 
}

