\name{ifa}
\alias{ifa}
\title{
Inverse of a matrix based on its factor decomposition.}
\description{
This function is not supposed to be called directly by users. It is needed in function \code{emfa} implementing an EM algorithm for a factor model.
}
\usage{
ifa(Psi, B)
}
\arguments{
  \item{Psi}{m-vector of specific variances, where m is the number of rows and columns of the matrix.}
  \item{B}{m x q matrix of loadings, where q is the number of factors}
}
\details{
If Sigma has the q-factor decomposition Sigma=diag(Psi)+BB', then Sigma^{-1} 
has the corresponding q-factor decomposition Sigma^{-1}=diag(phi)(I-theta theta')diag(phi), where theta is a mxq matrix and phi the vector of 
inverse specific standard deviations.}
\value{
\item{iS}{m x m inverse of diag(Psi)+BB'.}
\item{iSB}{m x q matrix (diag(Psi)+BB')^{-1}B.}
\item{Phi}{m-vector of inverse specific standard deviations.}
\item{Theta}{mxq matrix of loadings for the inverse factor model (see the details Section).}
}
\references{
Woodbury, M.A. (1949) The Stability of Out-Input Matrices. Chicago, Ill., 5 pp}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link{emfa}}, \code{\link{isqrtfa}}
}
\examples{
data(impulsivity)
erpdta = as.matrix(impulsivity[,5:505])   # erpdta contains the whole set of ERP curves     
fa = emfa(erpdta,nbf=20)          # 20-factor modelling of the ERP curves in erpdta
Sfa = diag(fa$Psi)+tcrossprod(fa$B) # Factorial estimation of the variance 
iSfa = ifa(fa$Psi,fa$B)$iS        # Matrix inversion    
max(abs(crossprod(Sfa,iSfa)-diag(ncol(erpdta)))) # Checks that Sfa x iSfa = diag(ncol(erpdta))
}