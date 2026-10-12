\name{MixSAL-package}
\alias{MixSAL-package}
\alias{MixSAL}
\docType{package}
\title{
Mixtures of SAL Distributions
}
\description{
The current version of the MixSAL package allows users to generate data from a multivariate SAL distribution or a mixture of multivariate SAL distributions, evaluate the probability density function of a multivariate SAL distribution or a mixture of multivariate SAL distributions, and fit a mixture of multivariate SAL distributions using the Expectation-Maximization (EM) algorithm (see Franczak et. al, 2014 for details).
}
\details{
\tabular{ll}{
Package: \tab MixSAL\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2018-05-09\cr
License: \tab GPL (>=3.1.3) \cr
}
This package contains the function {msal} for carrying about model based clustering using mixtures of SAL distributions; the functions {rsal} and {rmsal} for generating data from a multivariate SAL or mixture of multivariate SAL distributions, and hte functions {dsal} and {dmsal} for evaluating the model based clustering and classification using the mixture of generalized hyperbolic factor analyzers;
the function MCGHD for model based clustering  using the mixture of coalesced generalized hyperbolic distributions, and some real data sets.
}
\author{
Brian C. Franczak [aut, cre], Ryan P. Brown [aut, ctb], and Paul D. McNicholas [aut, ctb]

Maintainer: Brian C. Franczak <franczakb@macewan.ca>
}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.
}
\keyword{ Model-Based Clustering}
\keyword{ Finite Mixture Models }
\keyword{ Shifted Asymmetric Laplace Distributions}
\examples{
## Clustering Simulated Data
alpha <- matrix(c(2,2,1,2),2,2)
sig <- array(NA,dim=c(2,2,2))
sig[,,1] <- diag(2)
sig[,,2] <- matrix(c(1,0.5,0.5,1),2,2)
mu <- matrix(c(0,0,-2,5),2,2)
pi.g <- rep(1/2,2)
x <- rmsal(n=500,p=2,alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)

msal.ex1 <- msal(x=x[,-1],G=2)
table(x[,1],msal.ex1$cluster)

## Evaluate the probability density function of the specified mixture of SAL distributions
pdf.sal <- dmsal(x=x[,-1],alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)
pdf.sal[1:10]
}
