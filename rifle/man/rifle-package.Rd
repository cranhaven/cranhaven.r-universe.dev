\name{rifle-package}
\alias{rifle-package}
\docType{package}
\title{
Sparse Generalized Eigenvalue Problem
}
\description{
This package is called rifle. It implements algorithms for solving sparse generalized eigenvalue problem.  The algorithms are described in the paper "Sparse Generalized Eigenvalue Problem: Optimal Statistical Rates via Truncated Rayleigh Flow", by Tan et al. (2018). 

The main functions are as follows:
(1) initial.convex
(2) rifle

The first function, initial.convex, solves the sparse generalized eigenvalue problem using a convex relaxation.  The second function, rifle, refines the initial estimates from initial.convex and gives a more accurate estimator of the leading generalized eigenvector.   
}
\details{
The package includes the following functions:

\tabular{ll}{
	\code{\link{initial.convex}}: \tab Solve a convex relaxation of the sparse GEP \cr
	\code{\link{rifle}}: \tab Perform truncated rayleigh method to obtain the largest generalized eigenvector\cr
}
}

\author{
Kean Ming Tan

Maintainer: Kean Ming Tan 
}
\references{
Sparse Generalized Eigenvalue Problewm: Optimal Statistical Rates via Truncated Rayleigh Flow", by Tan et al. (2018). To appear in Journal of the Royal Statistical Society: Series B.  https://arxiv.org/pdf/1604.08697.pdf.
}

\keyword{ package }
\seealso{
	\code{\link{initial.convex}}
	\code{\link{rifle}}

}

\examples{

# Example on Fisher's Discriminant Analysis	on two class classification
# A small toy example
	n <- 50
	p <- 25

# Generate block diagonal covariance matrix with 5 blocks
	Sigma <- matrix(0,p,p)
	for(i in 1:p){
		Sigma[i,] <- 1:(p)-i
	}
	Sigma <- 0.7^abs(Sigma)

# Generate mean vector for two classes
	mu1 <- rep(0,p)
	mu2 <- c(rep(c(0,1),5),rep(0,p-10))

# Generate data for two classes
	X <- rbind(mvrnorm(n=n/2,mu1,Sigma),mvrnorm(n=n/2,mu2,Sigma))
	y <- rep(1:2,each=n/2)

# Estimate the subspace spanned by the largest eigenvector using convex relaxation
# Estimates
 estmu1 <- apply(X[y==1,],2,mean)
 estmu2 <- apply(X[y==2,],2,mean)
 estwithin <- cov(X[y==1,])+cov(X[y==2,])
 estbetween <- outer(estmu1,estmu1)+outer(estmu2,estmu2)

# Running initialization using convex relaxation
 a <- initial.convex(A=estbetween,B=estwithin,lambda=2*sqrt(log(p)/n),K=1,nu=1,trace=FALSE)

# Use rifle to improve the leading generalized eigenvector
 init <- eigen(a$Pi+t(a$Pi))$vectors[,1]

# Pick k such that the generalized eigenvector is sparse
 k <- 10
#  Rifle 1
 final.estimator <- rifle(estbetween,estwithin,init,k,0.01,1e-3)

# True direction in this simulation setting
# truebetween <- mu1 \%*\% t(mu1)+ mu2 \%*\% t(mu2)
# truewithin <- Sigma+Sigma
# temp <- eigen(truewithin)
# sqrtwithin <- temp$vectors \%*\% diag(sqrt(temp$values)) \%*\% t(temp$vectors)

# vecres <-svd(solve(sqrtwithin)\%*\% truebetween\%*\% solve(sqrtwithin))$v[,1]

# oracledirection <- solve(sqrtwithin) \%*\% vecres

# oracledirection <- oracledirection/sqrt(sum(oracledirection^2))

# Comparing estimated vs true direction by computing the cosine angle
# 1-sum(abs(oracledirection*final.estimator))

}
