\name{sparseBC-package}
\alias{sparseBC-package}
\docType{package}
\title{
Fit sparse biclustering and matrix-variate normal biclustering
}
\description{
This package is called sparseBC, for "Sparse biclustering".  It implements two methods:Sparse biclustering and matrix-variate normal biclustering.  All are described in the paper "Sparse biclustering of tranposable data", by KM Tan and D Witten (2014), \emph{Journal of Computational and Graphical Statistics}.

The main functions are as follows:
(1) sparseBC
(2) matrixBC

The first function, sparseBC, performs sparse biclustering.  matrixBC performs matrix-variate normal biclustering.  There are also cross-validation functions for tuning parameter that controls the sparsity level of the estimated mean matrix: sparseBC.BIC and matrixBC.BIC.  Function that choose the number of biclusters K and R are also included for sparseBC, called sparseBC.choosekr.   	
}
\details{
\tabular{ll}{
Package: \tab sparseBC\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-02-09\cr
License: \tab GPL (>=2.0) \cr
LazyLoad: \tab yes \cr
}

The package includes the following functions:

\tabular{ll}{
\code{\link{sparseBC}}: \tab Perform sparse biclustering\cr
\code{\link{sparseBC.choosekr}}: \tab Cross-validation to select the number of row and column clusters	\cr
\code{\link{sparseBC.BIC}}: \tab Select sparsity tuning parameter for sparseBC \cr
\code{\link{summary.sparseBC}}: \tab Display information for the object sparseBC \cr
\code{\link{image.sparseBC}}: \tab Image plot for the estimated bicluster mean matrix \cr
\code{\link{matrixBC}}:\tab Perform matrix-variate normal biclustering \cr
\code{\link{matrixBC.BIC}}:\tab Select sparsity tuning parameter for matrixBC\cr
}

}

\author{
Kean Ming Tan

Maintainer: Kean Ming Tan <keanming@u.washington.edu>
}
\references{
KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.
}
\seealso{
\code{\link{sparseBC}}
\code{\link{matrixBC}}
}
\keyword{ package }

\examples{
# An example that violates the assumption of contiguous biclusters
# Create mean matrix and the data matrix
#set.seed(5)
#u<-c(10,9,8,7,6,5,4,3,rep(2,17),rep(0,75))
#v<-c(10,-10,8,-8,5,-5,rep(3,5),rep(-3,5),rep(0,34))
#u<-u/sqrt(sum(u^2))
#v<-v/sqrt(sum(v^2))
#d<-50
#mus<-d*tcrossprod(u,v)
#binaryX<-(mus!=0)*1
#X<-mus+matrix(rnorm(100*50),100,50)
#X<-X-mean(X)

# The number of biclusters are chosen automatically
# Commented out for short run-time
#KR<-sparseBC.choosekr(X,1:6,1:6,0,0.1,trace=TRUE)
#k<-KR$estimated_kr[1]
#r<-KR$estimated_kr[2]

# The value of lambda is chosen automatically
#lambda<-sparseBC.BIC(X,k,r,c(0,10,20,30,40,50))$lambda

# Perform sparse biclustering using the K, R, and lambda chosen
#biclustering<-sparseBC(X,k,r,lambda)

# Display some information on the object sparseBC
#summary(biclustering)

# Plot the estimated mean matrix from sparseBC
#image(biclustering)
}
