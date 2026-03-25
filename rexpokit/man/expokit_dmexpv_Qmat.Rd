\name{expokit_dmexpv_Qmat}
\alias{expokit_dmexpv_Qmat}
\title{EXPOKIT dmexpv matrix exponentiation on Q matrix}
\usage{
  expokit_dmexpv_Qmat(Qmat = NULL, t = 2.1,
    inputprobs_for_fast = NULL, transpose_needed = TRUE,
    transform_to_coo_TF = TRUE, coo_n = NULL, anorm = NULL,
    check_for_0_rows = TRUE)
}
\arguments{
  \item{Qmat}{an input Q transition matrix}

  \item{t}{one or more time values to exponentiate by}

  \item{inputprobs_for_fast}{If NULL (default), the full
  probability matrix (Pmat) is returned. However, the full
  speed of EXPOKIT on sparse matrices will be exploited if
  inputprobs_for_fast=c(starting probabilities). In this
  case these starting probabilities are input to
  \code{myDMEXPV} directly, as \code{v}, and \code{w}, the
  output probabilities, are returned.}

  \item{transpose_needed}{If TRUE (default), matrix will be
  transposed (apparently EXPOKIT needs the input matrix to
  be transposed compared to normal)}

  \item{transform_to_coo_TF}{Should the matrix be tranposed
  to COO?  COO format is required for EXPOKIT's
  sparse-matrix functions (like dmexpv and unlike the
  padm-related functions. Default TRUE; if FALSE, user must
  put a COO-formated matrix in \code{Qmat}. Supplying the
  coo matrix is probably faster for repeated calculations
  on large matrices.}

  \item{coo_n}{If a COO matrix is input, \code{coo_n}
  specified the order (# rows, equals # columns) of the
  matrix.}

  \item{anorm}{\code{dmexpv} requires an initial guess at
  the norm of the matrix. Using the}

  \item{check_for_0_rows}{If TRUE or a numeric value, the
  input Qmat is checked for all-zero rows, since these will
  crash the FORTRAN wrapalldmexpv function. A small nonzero
  value set to check_for_0_rows or the default
  (0.0000000000001) is input to off-diagonal cells in the
  row (and the diagonal value is normalized), which should
  fix the problem. R function \code{\link{norm}} might get
  slow with large matrices. If so, the user can input a
  guess manually (\code{Lagrange} seems to just use 1 or 0,
  if I recall correctly).}
}
\value{
  \code{tmpoutmat} the output matrix. \code{wrapalldmexpv_}
  produces additional output relating to accuracy of the
  output matrix etc.; these can be by a direct call of
  dmexpv.
}
\description{
  This function converts a matrix to COO format and
  exponentiates it via the EXPOKIT dmexpv function
  (designed for sparse matrices) and wrapper functions
  \code{wrapalldmexpv_} around dmexpv.
}
\details{
  From EXPOKIT:\cr \code{* The method used is based on
  Krylov subspace projection}\cr \code{* techniques and the
  matrix under consideration interacts only}\cr \code{* via
  the external routine 'matvec' performing the
  matrix-vector} \cr \code{* product (matrix-free
  method).}\cr \code{*}\cr \code{* This is a customised
  version for Markov Chains. This means that a}\cr \code{*
  check is done within this code to ensure that the
  resulting vector} \cr \code{* w is a probability vector,
  i.e., w must have all its components }\cr \code{* in
  [0,1], with sum equal to 1. This check is done at some
  expense}\cr \code{* and the user may try DGEXPV which is
  cheaper since it ignores }\cr \code{* probability
  constraints.}\cr

  COO (coordinated list) format is a compressed format that
  is\cr required for EXPOKIT's sparse-matrix functions
  (like dmexpv and\cr unlike EXPOKIT's padm-related
  functions.\cr

  COO (coordinated list) format is described here:\cr

  \url{https://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr

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

# Exponentiate each with EXPOKIT's dmexpv (should be fast for large sparse matrices)
for (t in tvals)
	{
	Pmat = expokit_dmexpv_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
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

  \code{\link{expokit_wrapalldmexpv_tvals}}
}

