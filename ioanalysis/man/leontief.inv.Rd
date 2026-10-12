\name{leontief.inv}
\alias{leontief.inv}
\title{Leontief Inverse}

\description{
Computes the Leontief (input) inverse. \code{leontief.inv} has inputs to invert a subset of all regions if desired. If not using an \code{InputOutput} object from \code{\link{as.inputoutput}}, the functionality is limited. See example for more details.

Note: if you have a non \code{InputOutput} object and you wish to use only a subset of all regions, you must supply the intermediate transaction matrix (\code{Z}) and total production matrix (\code{X}). Otherwise use \code{L <- Z \%*\% diag(c(1/X))}

  Caution: Inverting large matrices will take a long time. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds.}

\usage{
leontief.inv(Z = NULL, X, A, RS_label, regions)
}

\arguments{
  \item{Z}{Either an object class of \code{InputOutput} calculated from \code{\link{as.inputoutput}} or the intermediate transaction matrix. Do NOT use matrix of technical coefficients.}
  \item{X}{vector. Total production vector. Not required if Z is an object with \code{InputOutput} class.}
  \item{A}{Matrix. Technical Matrix of Input Coefficients. If provided and the data is large, the computations will be noticeably sped up.}
  \item{RS_label}{Matrix. A nx2 column matrix of labels for regions and sectors. The first column must be regions and the second column must be sectors. This is used to match with the intermediate transaction matrix.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
}

\details{
The Leontief inverse is derived from the input-output table A=[a_ij] where
\deqn{a_ij=z_ij/X_j}

where z_ij is the input from i required in the production of j. X_j is the corresponding input in each column. The Leontief inverse is then computed as
\deqn{(I-A)^{-1}}

Observe we result with the following system
\deqn{X = Lf}
Therefore, element \eqn{l_{ij}} is interpreted as the ratio of final demand for sector j contributing to the total production in sector i.
}

\value{
Returns a matrix with the Leontief Inverse.
}
\references{
Leontief, Wassily W. (1951). "Input-Output Economics." \emph{Scientific American}, Vol. 185, No. 4 (October 1951), pp. 15-21.

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})

}
\author{
Ignacio Sarmiento-Barbieri, John J. P. Wade
}


\examples{
# Using an "InputOutput" object
data(toy.IO)
class(toy.IO)

L1 <- leontief.inv(toy.IO, region = "Narnia")

# Otherwise
Z <- toy.IO$Z
X <- toy.IO$X
L2 <- leontief.inv(Z, X)
}
