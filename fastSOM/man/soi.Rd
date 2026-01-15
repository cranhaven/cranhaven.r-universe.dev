\name{soi}
\Rdversion{1.1}
\alias{soi}
\title{Calculation of the Spillover Index}
\description{
  This function calculates the spillover index as proposed by Diebold and Yilmaz (2009, see References). 
}
\usage{soi(Sigma, A, ncores = 1, ...)}
\arguments{
  \item{Sigma}{
    Either a covariance matrix or a list thereof.
  }
  \item{A}{ 
    Either a 3-dimensional array with A[,,h] being MA coefficient matrices of the same dimension as \code{Sigma} or a list thereof.
  }
  \item{ncores}{
    Number of cores, only relevant if Sigma is a list of matrices. 
    Missing ncores or \code{ncores=1} means no parallelization (just one core is used). 
    \code{ncores=0} means automatic detection of the number of available cores. 
    Any other integer determines the maximal number of cores to be used.
  }
  \item{\dots}{ 
    Further arguments, especially \code{perm} which is used to reorder variables.
    If \code{perm} is missing, then the original 
    ordering of the model variables will be used.
    If \code{perm} is a permutation of \code{1:N}, then the spillover index for the model with variables reordered according to perm will be calculated.
  }
}
\details{
  The spillover index was introduced by Diebold and Yilmaz in 2009 (see References). It is 
  based on a variance decompostion of the forecast error variances of an \eqn{N}-dimensional MA(\eqn{\infty}) process. 
  The underlying idea is to decompose the forecast error of each variable into own variance shares 
  and cross variance shares. The latter are interpreted as contributions of shocks of one variable
  to the error variance in forecasting another variable (see also \code{\link{sot}}). 
  The spillover index then is a number between 0 and 100, describing the relative amount of forecast error variances that can
  be explained by shocks coming from other variables in the model.
  
  The typical application of the 'list' version of \code{soi} is a rolling windows approach when \code{Sigma} and \code{A} are lists representing the corresponding quantities at different points in time
  (rolling windows). 
}
\value{
  Returns a single numeric value or a list thereof.
}
\references{
  [1] Diebold, F. X. and Yilmaz, K. (2009):  \href{http://onlinelibrary.wiley.com/doi/10.1111/j.1468-0297.2008.02208.x/pdf}{Measuring financial asset return and volatitliy spillovers, 
                                             with application to global equity markets}, 
                                             Economic Journal 199(534): 158-171.
                                             
  [2] Kloessner, S. and Wagner, S. (2012):  \href{http://onlinelibrary.wiley.com/doi/10.1002/jae.2366/pdf}{Exploring All VAR Orderings for Calculating Spillovers? Yes, We Can! - 
											A Note on Diebold and Yilmaz (2009)}, Journal of Applied Econometrics 29(1): 172-179
}
\author{
  Stefan Kloessner (\email{S.Kloessner@mx.uni-saarland.de}), \cr
  with contributions by Sven Wagner (\email{sven.wagner@mx.uni-saarland.de})
}
\seealso{ \code{\link{fastSOM-package}}, \code{\link{sot}} }
\examples{
# generate randomly positive definite matrix Sigma of dimension N 
N <- 10
Sigma <- crossprod(matrix(rnorm(N*N),nrow=N)) 
# generate randomly coefficient matrices
H <- 10 
A <- array(rnorm(N*N*H),dim=c(N,N,H)) 
# calculate the spillover index
soi(Sigma, A)
}
\keyword{spillover index}