\name{expm}
\alias{expm}
\title{Simple Interface}
\usage{
  expm(x, t=1)
}
\arguments{
  \item{x}{A square matrix.}
  \item{t}{Time value; see details section below. Default value is 1.}
}
\value{
  The matrix exponential.
}
\description{
  A simple interface for matrix exponentiation, similar to that found
  in the Matrix package.
}
\details{
  This routine computes exp(t*x), where \code{t} in a single numeric value,
  and x is a square matrix and exp refers to the matrix exponential.
  Formally, the matrix exponential is given by the power series:
  
  \eqn{expm(x) = x/1! + x^2/2! + x^3/3! + \dots}
  
  where the powers on the matrix correspond to matrix-matrix multiplications.
  
  \code{expm()} directly computes the matrix exponential of a dense 
  matrix. The implementation uses an order 6 Pade' approximation with a 
  scaling-and-squaring technique.
}
\references{
  Sidje RB (1998). "Expokit. A Software Package for
  Computing Matrix Exponentials." _ACM Trans. Math.
  Softw._, *24*(1), pp. 130-156. <URL:
  http://dx.doi.org/10.1145/285861.285868>, <URL:
  \url{https://dl.acm.org/doi/10.1145/285861.285868}>.
}
\examples{
# Example use:
x <- matrix(1:25, nrow=5)/100

expm(x)
}
\author{
  Drew Schmidt \email{schmidt@math.utk.edu}
}


