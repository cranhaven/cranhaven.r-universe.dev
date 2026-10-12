\name{isqrtfa}
\alias{isqrtfa}
\title{Inverse square-root of a matrix based on its factor decomposition.}
\description{
This function is not supposed to be called directly by users. It is needed in functions \code{erpfatest} and \code{erpFtest} implementing factor-adjusted testing methods.
}
\usage{
isqrtfa(Psi, B)
}
\arguments{
  \item{Psi}{m-vector of specific variances, where m is the number of rows and columns of the matrix.}
  \item{B}{mxq matrix of loadings, where q is the number of factors}
}
\details{
If Sigma has the q-factor decomposition Sigma=diag(Psi)+BB', then the function returns a matrix Omega such that Sigma^{-1} = Omega Omega'. Equivalently,
Omega' Sigma Omega = I.}
\value{
The mxm matrix Omega (see the details Section).
}
\references{
Woodbury, M.A. (1949) The Stability of Out-Input Matrices. Chicago, Ill., 5 pp}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link{emfa}}, \code{\link{ifa}}
}
\examples{
data(impulsivity)
erpdta = as.matrix(impulsivity[,5:505])   # erpdta contains the whole set of ERP curves     
fa = emfa(erpdta,nbf=20)          # 20-factor modelling of the ERP curves in erpdta
Sfa = diag(fa$Psi)+tcrossprod(fa$B) # Factorial estimation of the variance 
iSfa = ifa(fa$Psi,fa$B)$iS        # Matrix inversion    
isqrtSfa = isqrtfa(fa$Psi,fa$B)   # Inverse square-root of Sfa
max(abs(tcrossprod(isqrtSfa)-iSfa)) # Checks that isqrtSfa x t(isqrtSfa) = iSfa
}

