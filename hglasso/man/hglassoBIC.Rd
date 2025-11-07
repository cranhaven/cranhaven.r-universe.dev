\name{hglassoBIC}
\alias{hglassoBIC}
\title{
BIC-type criterion for \code{hglasso}
}
\description{
This function calculates the BIC-type criterion for tuning parameter selection for \code{\link{hglasso}} proposed in Section 3.4 in Tan et al. (2014)  
}
\usage{
hglassoBIC(x, S, c=0.2)
}

\arguments{
  \item{x}{
An object of class \code{\link{hglasso}}.}
  \item{S}{
A p by p correlation/covariance matrix.  Cannot contain missing values.}
  \item{c}{
A constant between 0 and 1.  When c is small, the BIC-type criterion will favor more hub nodes. The default value is c=0.2.
}
}

\value{
\item{BIC}{The calculated BIC-type criterion in Section 3.4 in Tan et al. (2014).}
}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.
}
\author{
Kean Ming Tan}

\seealso{
  \code{\link{hglasso}}
  }
\examples{
#library(mvtnorm)
#library(glasso)
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
#S <- cov(x)
# Run Hub Graphical Lasso with different tuning parameters
#lambdas2 <- seq(0,0.5,by=0.05)
#BICcriterion <- NULL
#for(lambda2 in lambdas2){
#res1 <- hglasso(S,0.3,lambda2,1.5)
#BICcriterion <- c(BICcriterion,hglassoBIC(res1,S)$BIC)
#}
#lambda2 <- lambdas2[which(BICcriterion==min(BICcriterion))]
}


