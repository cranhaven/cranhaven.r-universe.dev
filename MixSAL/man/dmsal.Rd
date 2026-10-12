\name{dmsal}
\alias{dmsal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability Density Function for a Mixture of SAL Distributions
}
\description{
Evaluates the probability density function of a mixture of multivariate SAL distribution.
}
\usage{
dmsal(x, alpha, sig, mu, pi.g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A n by p matrix where each row corresponds a p-dimensional observation.
}
  \item{alpha}{
A matrix where each row specifies the direction of skewness in each variable for each mixture component.
}
  \item{sig}{
An array where each matrix specifies the covariance matrix for each mixture component.
}
  \item{mu}{
A matrix where each row gives the mean vector for each mixture component.
}
  \item{pi.g}{
A vector specifying the mixing components.
}
}
\value{
A vector of length n that gives the value of the probability density function for each observation in the matrix x and the specified parameter values.
}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.
}
\author{
Brian C. Franczak [aut, cre], Ryan P. Browne [aut, ctb], Paul D. McNicholas [aut, ctb]

Maintainer: Brian C. Franczak <franczakb@macewan.ca>
}

\examples{
## For this illustration, consider the following dataset generated from a mixture of bivariate SAL
##distributions with the specified parameter set:
alpha <- matrix(c(2,2,1,2),2,2)
sig <- array(NA,dim=c(2,2,2))
sig[,,1] <- diag(2)
sig[,,2] <- matrix(c(1,0.5,0.5,1),2,2)
mu <- matrix(c(0,0,-2,5),2,2)
pi.g <- rep(1/2,2)
x <- rmsal(n=10,p=2,alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)
## The value of the probability density function for each of the simulated values are given by:
dmsal(x=x[,-1],alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Probability Density Function}% use one of  RShowDoc("KEYWORDS")
\keyword{Mixtures of SAL Distributions}% __ONLY ONE__ keyword per line