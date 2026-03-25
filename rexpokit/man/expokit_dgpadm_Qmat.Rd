\name{expokit_dgpadm_Qmat}
\alias{expokit_dgpadm_Qmat}
\title{EXPOKIT dgpadm matrix exponentiation on Q matrix}
\usage{
  expokit_dgpadm_Qmat(Qmat = NULL, t = 2.1,
    transpose_needed = TRUE)
}
\arguments{
  \item{Qmat}{an input Q transition matrix}

  \item{t}{one or more time values to exponentiate by}

  \item{transpose_needed}{If TRUE (default), matrix will be
  transposed (apparently EXPOKIT needs the input matrix to
  be transposed compared to normal)}
}
\value{
  \code{tmpoutmat} the output matrix. \code{wrapalldmexpv_}
  produces additional output relating to accuracy of the
  output matrix etc.; these can be obtained by a direct
  call of wrapalldmexpv_.
}
\description{
  This function exponentiates a matrix via the EXPOKIT padm
  function (designed for small dense matrices) and wrapper
  function \code{wrapalldgpadm_} around dmexpv.\cr
}
\details{
  From EXPOKIT:\cr

  \code{* Computes exp(t*H), the matrix exponential of a
  general matrix in }\cr \code{* full, using the
  irreducible rational Pade approximation to the }\cr
  \code{* exponential function exp(x) = r(x) = (+/-)( I +
  2*(q(x)/p(x)) ), }\cr \code{* combined with
  scaling-and-squaring.  }\cr

  If \code{Qmat} is NULL (default), a default matrix is
  input.\cr
}
\examples{
# Example:
# Make a square instantaneous rate matrix (Q matrix)
# This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
# to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
# Unleashed" at:
# \\url{http://www.bioinf.org/molsys/data/idiots.pdf}
#
# The Q matrix includes the stationary base freqencies, which Pmat
# converges to as t becomes large.
Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168,
0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)

# Make a series of t values
tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)

# Exponentiate each with EXPOKIT's dgpadm (good for small dense matrices)
for (t in tvals)
	{
	Pmat = expokit_dgpadm_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
	cat("\\n\\nTime=", t, "\\n", sep="")
	print(Pmat)
	}
}
\author{
  Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com} and Drew Schmidt
  \email{schmidt@math.utk.edu}
}
\seealso{
  \code{\link{mat2coo}}
}

