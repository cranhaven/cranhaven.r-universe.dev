\name{soi_from_sot}
\Rdversion{1.1}
\alias{soi_from_sot}
\title{
  Calculation of the Spillover Index for a given Spillover Table
}
\description{
  Given a spillover table, this function calculates the corresponding spillover index.
}
\usage{soi_from_sot(input_table)}
\arguments{
  \item{input_table}{
    Either a spillover table or a list thereof
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
  
  The typical application of the 'list' version of \code{soi_from_sot} is a rolling windows approach when \code{input_table} is a list representing the corresponding spillover tables at different points in time
  (rolling windows). 
}
\value{
  Numeric value or a list thereof.
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
\seealso{\code{\link{fastSOM-package}}, \code{\link{soi}}, \code{\link{sot}} }
\examples{
# generate randomly positive definite matrix Sigma of dimension N 
N <- 10
Sigma <- crossprod(matrix(rnorm(N*N),nrow=N)) 
# generate randomly coefficient matrices
H <- 10 
A <- array(rnorm(N*N*H),dim=c(N,N,H)) 
# calculate spillover table
SOT <- sot(Sigma,A) 
# calculate spillover index from spillover table
soi_from_sot(SOT) 
}
\keyword{spillover index from spillover table}