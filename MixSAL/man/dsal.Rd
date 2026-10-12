\name{dsal}
\alias{dsal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability Density Function for a Multivariate SAL Distribution
}
\description{
Evaluates the probability density function of a multivariate SAL distribution.
}
\usage{
dsal(x, alpha, sig, mu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A n by p matrix where each row corresponds a p-dimensional observation.
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
A vector of length n that gives the value of the probability density function for each observation in the matrix x and the specified parameter values.
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
## For this illustration, consider bivariate SAL data from the specified distribution:
x <- rsal(n=10,p=2,alpha=c(2,2),sig=diag(2),mu=c(0,0))
## The value of the probability density function for each of the simulated values are given by:
dsal(x=x,alpha=c(2,2),sig=diag(2),mu=c(0,0))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Probability Density Function}% use one of  RShowDoc("KEYWORDS")
\keyword{Multivariate SAL Distribution}% __ONLY ONE__ keyword per line
