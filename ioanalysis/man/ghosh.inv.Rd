\name{ghosh.inv}
\alias{ghosh.inv}
\title{Ghoshian Inverse}

\description{
Computes the Ghoshian (ouput) inverse. \code{ghosh.inv} has inputs to invert a subset of all regions if desired. If not using an \code{InputOutput} object from \code{\link{as.inputoutput}}, the functionality is limited. See example for more details.

  Caution: Inverting large matrices will take a long time. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds. }

\usage{
ghosh.inv(Z = NULL, X, B, RS_label, regions)
}

\arguments{
  \item{Z}{Either an object class of \code{InputOutput} calculated from \code{\link{as.inputoutput}} or the intermediate transaction matrix. Do NOT use matrix of technical coefficients.}
  \item{X}{Vector. Total production vector. Not required if Z is an object with \code{InputOutput} class.}
  \item{B}{Matrix. Matrix of technical output coefficients.}
  \item{RS_label}{Matrix. A nx2 column matrix of labels for regions and sectors. The first column must be regions and the second column must be sectors. This is used to match with the intermediate transaction matrix.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
}

\details{
The Ghoshian inverse is derived from the input-output table A=[a_ij] where
\deqn{b_ij=z_ij/X_i}

where z_ij is the input from i required in the production of j. X_i is the corresponding input in each row. The Leontief inverse is then computed as
\deqn{(I-B)^{-1}}

Observe we result with the following system
\deqn{X'=V'G}
Therefore, the element \eqn{g_{ij}} is interpreted as the ratio of sector i's value added contributing to the total production of sector j.
}

\value{
Returns a matrix with the Ghoshian Inverse
}
\references{
Ghosh, A. (1958). "Input-output Approach in an Allocation System," \emph{Econometrica}, New Series, Vol. 25, No. 97 (Feb., 1958), pp. 58-64.

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})

}
\author{
Ignacio Sarmiento-Barbieri, John J. P. Wade
}


\examples{
# Using an "InputOutput" object
data(toy.IO)
class(toy.IO)

G1 <- ghosh.inv(toy.IO, region = "Narnia")

# Otherwise
Z <- toy.IO$Z
X <- toy.IO$X
G3 <- ghosh.inv(Z, X)
}
