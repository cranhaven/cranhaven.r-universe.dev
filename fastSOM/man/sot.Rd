\name{sot}
\Rdversion{1.1}
\alias{sot}
\title{Calculation of Spillover Tables}
\description{
  This function calculates an \eqn{N x N}-dimensional spillover table. 
}
\usage{sot(Sigma, A, ncores = 1, ...)}
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
  The \eqn{(i,j)}-entry of a spillover table represents the relative contribution of shocks in variable \eqn{j} 
  (the column variable) to the forecasting error variance of variable \eqn{i} (the row variable). 
  Hence, off-diagonal values are interpreted as spillovers, while the own variance shares appear on the
  diagonal. An overall spillover measure is given by \code{\link{soi}}. 
  
  The typical application of the 'list' version of \code{sot} is a rolling windows approach when \code{Sigma} and \code{A} are lists representing the corresponding quantities at different points in time
  (rolling windows). 
}
\value{ 
  Matrix, or a list thereof, of dimensions \eqn{N x N} with non-negative entries summing up to 100 for each row.}
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
\seealso{ \code{\link{fastSOM-package}}, \code{\link{soi}} }
\examples{
# generate randomly positive definite matrix Sigma of dimension N 
N <- 10
Sigma <- crossprod(matrix(rnorm(N*N),nrow=N)) 
# generate randomly coefficient matrices
H <- 10 
A <- array(rnorm(N*N*H),dim=c(N,N,H)) 
# calculate spillover table
sot(Sigma,A) 
}
\keyword{spillover table}