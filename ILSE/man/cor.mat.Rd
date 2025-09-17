\name{cor.mat}
\alias{cor.mat}
\title{Generate Two Type of Correlation Matrix}
\description{
  Generate two type of correlation matrix
}
\usage{
  cor.mat(p, rho, type='toeplitz')
}
\arguments{
  \item{p}{a positive integer, the dimension of correlation matrix.}
  \item{rho}{a value between 0 and 1, a baseline vlaue of correlation coefficient.}
  \item{type}{a character, specify the type of correlation matrix and only include
  'toeplitz' and 'identity' in current version.}
}
\details{
  The argument rho specify the size of correlation coeffient. As for argument type, if type='toeplitz', sigma_ij=rho^{|i-j|}; if type ='identity', sigma_{ij}=rho when i!=j and sigma_{ij}=1 when i=j.
}
\value{
  return a correlation matrix with a type of specified structure.
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
  cor.mat(5, 0.5)
  cor.mat(5, 0.5, type='identity')
}

