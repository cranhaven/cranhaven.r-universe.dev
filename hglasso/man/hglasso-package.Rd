\name{hglasso-package}
\alias{hglasso-package}
\docType{package}
\title{
Fit the hub graphical lasso, hub covariance graph, and hub binary network
}
\description{
This package is called hglasso, for "hub graphical lasso".  It implements three methods:hub graphical lasso, hub covariance graph, and hub binary network.  All are described in the paper "Learning graphical models with hubs", by Tan et al. (2014).

The main functions are as follows:
(1) hglasso
(2) hcov
(3) hbn

The first function, hglasso, performs hub graphical lasso.  The second function, hcov, performs hub covariance graph estimation.  The third function, hbn, performs hub binary network estimation.   

}
\details{
\tabular{ll}{
Package: \tab hglasso\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-08-09\cr
License: \tab GPL (>=2.0) \cr
LazyLoad: \tab yes \cr
}

The package includes the following functinos:

\tabular{ll}{
\code{\link{hglasso}}: \tab Performs hub graphical lasso\cr
\code{\link{hcov}}:\tab Performs hub covariance graph estimation \cr
\code{\link{hbn}}:\tab Performs hub binary network estimation \cr
\code{\link{HubNetwork}}:\tab Generates inverse covariance matrix or covariance matrix with hubs \cr
\code{\link{binaryMCMC}}:\tab Generates samples for binary Ising model via Gibbs sampling \cr
\code{\link{image.hglasso}}: \tab Creates image plot of the matrix V and Z\cr
\code{\link{plot.hglasso}}: \tab Creates a graphical representation of the estimated matrix Theta \cr
\code{\link{summary.hglasso}}: \tab Provides summary for the matrix Theta, Z, and V  \cr
\code{\link{hglassoBIC}}: \tab Calculate BIC-type criterion for \code{\link{hglasso}} \cr
}
}
\author{
Kean Ming Tan and Karthik Mohan

Karthik Mohan implemented the Barzilai-Borwein method for \code{\link{hbn}}

Maintainer: Kean Ming Tan <keanming@uw.edu>

}
\references{
Tan, KM., London, P., Mohan, K., Lee, S-I., Fazel, M., and Witten, D. (2014). Learning graphical models with hubs. Journal of Machine Learning Research 15(Oct):3297-3331.
}
\keyword{ package }

\seealso{
\code{\link{hglasso}}
\code{\link{hcov}}
\code{\link{hbn}}
}

\examples{
##############################################
# Example from Figure 1 in the manuscript
# A toy example to illustrate the results from 
# Hub Graphical Lasso
##############################################
#library(mvtnorm)
#set.seed(1)
#n=100
#p=100

# A network with 4 hubs
#network<-HubNetwork(p,0.99,4,0.1)
#Theta <- network$Theta
#truehub <- network$hubcol
# The four hub nodes have indices 14, 42, 45, 78
#print(truehub)

# Generate data matrix x
#x <- rmvnorm(n,rep(0,p),solve(Theta))
#x <- scale(x)

# Run Hub Graphical Lasso to estimate the inverse covariance matrix
# res1<-hglasso(cov(x),0.3,0.3,1.5)

# print out a summary of the object hglasso
#summary(res1)
# we see that the estimated hub nodes have indices 14, 42, 45, 78
# We successfully recover the 4 hub nodes

# Plot the matrices V and Z 
#image(res1)
#dev.off()
# Plot a graphical representation of the estimated inverse
# covariance matrix --- conditional independence graph
#plot(res1,main="Conditional Independence Graph")

}

