\name{GetVarCov}
\alias{GetVarCov}
\title{
Asymptotic variance-covariance of the estimators
}
\description{
Obtain the asymptotic covariance metrix for the combined covariance estimate. you need to run the \code{CovComb} first and then use the estimated covariace matrix as an input to this function. 
}
\usage{
GetVarCov(Hmat, Klist, nu = 100, w=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{Hmat}{
The estimated covariace matrix. Output from \code{CovComb}.
}
  \item{Klist}{
A list of covariance / relationship matrices with row and column names to be combined.
}
  \item{w}{
Weight parameter, a vector of the same length as Klist, elements corresponding to weights assigned to each of the covariance matrices. Default is 1. 
}
  \item{nu}{
Degrees of freedom parameter. It is either a scalar (same degrees of freeom to each of the covariance component) or a vector of the same length as Klist elements of which correspond to each of the covariance matrices. Currently, only scalar nu is accepted. Default is 1000. the value of nu needs to be larger than the variables in the covariance matrix.
}
}

\value{
Asymptotic sampling covariance matrix for the combined covariance estimate. The diagonals elements correspond to the sampling variances of the covariance estimates.  
}
\references{
- Adventures in Multi-Omics I: Combining heterogeneous data sets via relationships matrices. Deniz Akdemir, Julio Isidro Sanchez. bioRxiv, November 28, 2019
}
\author{Deniz Akdemir // Maintainer: Deniz Akdemir
\email{deniz.akdemir.work@gmail.com}
}

\examples{
data("mtcars")
my_data <- mtcars[, c(1,3,4,5)]
dim(my_data)
# print the first few rows
head(my_data)
#ArtificiaLly making 3 partial covariance matrices! 
#These are the partial covariances obtained from 
#independent  multi-view experiments.
set.seed(123)
cov12<-cov(my_data[sample(nrow(my_data),20),1:2])
cov23<-cov(my_data[sample(nrow(my_data),20),2:3])
cov34<-cov(my_data[sample(nrow(my_data),20),3:4])

# Combine covariances using the package
Combined<-CovComb(Klist=list(cov12,cov23,cov34))
# Get asyptotic sampling variance- covariance matrix.  
SEMAT<-GetVarCov(Hmat=Combined,
Klist=list(cov12,cov23,cov34),nu=20,w=1)
## Square root of the diagonal elements are 
## the asymptotic standard errors. 
round(sqrt(diag(SEMAT)),3)
}

\keyword{multivariate}
\keyword{models}
\keyword{regression}
