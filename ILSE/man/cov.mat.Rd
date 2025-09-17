\name{cov.mat}
\alias{cov.mat}
\title{Generate Two Type of Covariance Matrix}
\description{
  Generate two type of covariance matrix
}
\usage{
  cov.mat(sdvec,rho, type='toeplitz')
}
\arguments{
  \item{sdvec}{a positive vector, standard deviation of each random variable.}
  \item{rho}{a value between 0 and 1, a baseline vlaue of correlation coefficient.}
  \item{type}{a character, specify the type of correlation matrix and only include
    'toeplitz' and 'identity' in current version.}
}
\details{
  The argument rho specify the size of correlation coeffient. As for argument type, if type='toeplitz', sigma_ij=rho^{|i-j|}; if type ='identity', sigma_{ij}=rho when i!=j and sigma_{ij}=1 when i=j.
}
\value{
  return a covariance matrix with a type of specified structure.
}
\references{
  nothing.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  cov2cor
}
\examples{
  cov.mat(rep(5,5), 0.5)
  cov.mat(c(2,4,3), 0.5, type='identity')
}
