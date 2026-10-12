\name{rsal}
\alias{rsal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate from a Multivariate SAL Distribution
}
\description{
Generates data from a multivariate shifted asymmetric Laplace (SAL) distributions.
}
\usage{
rsal(n, p, alpha, sig, mu)
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
A vector specifying the direction of skewness in each variable.
}
  \item{sig}{
A matrix specifying the covariance matrix of the variables.
}
  \item{mu}{
A vector specifiying the mean vector. 
}
}
\value{
An n by p matrix where each row corresponds to one observation from the specified multivariate SAL distribution. 
}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.

Kotz et. al (2001). The Laplace Distribution and Generalizations: A Revisit with Applications to Communications. Economics, Engineering, and Finance. 1st Edition, Burkhauser.
}
\author{
Brian C. Franczak [aut, cre], Ryan P. Browne [aut, ctb], Paul D. McNicholas [aut, ctb]

Maintainer: Brian C. Franczak <franczakb@macewan.ca>
}
\examples{
x <- rsal(n=500,p=2,alpha=c(2,2),sig=diag(2),mu=c(0,0)) 
plot(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}% use one of  RShowDoc("KEYWORDS")
\keyword{Multivariate SAL Distribution}% __ONLY ONE__ keyword per line
