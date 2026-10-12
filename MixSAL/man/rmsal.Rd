\name{rmsal}
\alias{rmsal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate from a Mixture of Multivariate SAL Distributions
}
\description{
Generates data from a mixture of multivariate shifted asymmetric Laplace (SAL) distributions.
}
\usage{
rmsal(n, p, alpha, sig, mu, pi.g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The number of observations required.
}
  \item{p}{
The dimension of the data.
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
An n by p + 1 matrix where each row corresponds to one observation from the specified mixture of SAL distributions. The first column gives the component (or group) label for each observation and columns 2 to p + 1 give the values of the p-dimensional observation. 
}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.
}
\author{
Brian C. Franczak [aut, cre], Ryan P. Browne [aut, ctb], Paul D. McNicholas [aut, ctb]

Maintainer: Brian C. Franczak <franczakb@macewan.ca>
}
\examples{
alpha <- matrix(c(2,2,1,2),2,2)
sig <- array(NA,dim=c(2,2,2))
sig[,,1] <- diag(2)
sig[,,2] <- matrix(c(1,0.5,0.5,1),2,2)
mu <- matrix(c(0,0,-2,5),2,2)
pi.g <- rep(1/2,2)
x <- rmsal(n=500,p=2,alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)
plot(x[,-1],col=x[,1],pch=x[,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}% use one of  RShowDoc("KEYWORDS")
\keyword{Mixtures of SAL Distributions}% __ONLY ONE__ keyword per line
