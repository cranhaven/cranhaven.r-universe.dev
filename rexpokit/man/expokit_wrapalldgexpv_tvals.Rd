\name{expokit_wrapalldgexpv_tvals}
\alias{expokit_wrapalldgexpv_tvals}
\title{Run EXPOKIT's dgexpv on one or more t-values}
\usage{
  expokit_wrapalldgexpv_tvals(Qmat = NULL, tvals = c(2.1),
    inputprobs_for_fast = NULL, transpose_needed = TRUE,
    transform_to_coo_TF = TRUE, coo_n = NULL,
    force_list_if_1_tval = FALSE, check_for_0_rows = TRUE)
}
\arguments{
  \item{Qmat}{an input Q transition matrix}

  \item{tvals}{one or more time values to exponentiate by
  (doesn't have to literally be a time value, obviously)}

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

  \item{force_list_if_1_tval}{Default FALSE, but set to
  TRUE if you want a single matrix to be returned inside a
  list}

  \item{check_for_0_rows}{If TRUE or a numeric value, the
  input Qmat is checked for all-zero rows, since these will
  crash the FORTRAN wrapalldmexpv function. A small nonzero
  value set to check_for_0_rows or the default
  (0.0000000000001) is input to off-diagonal cells in the
  row (and the diagonal value is normalized), which should
  fix the problem.}
}
\value{
  \code{tmpoutmat} the output matrix, if 1 t-value is
  input; \code{list_of_matrices_output}, if more than 1
  t-value is input; to get a single output matrix in a
  list, set \code{force_list_if_1_tval=TRUE}
}
\description{
  The function runs EXPOKIT's \code{dgexpv} function on a Q
  matrix and \emph{one or more} time values.  If
  \code{Qmat} is NULL (default), a default matrix is
  input.\cr
}
\details{
  NOTE: DGEXPV vs. DMEXPV. According to the EXPOKIT
  documentation, DGEXPV should be faster than DMEXPV,
  however DMEXPV runs an accuracy check appropriate for
  Markov chains, which is not done in DGEXPV.\cr
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

# Exponentiate each with EXPOKIT's dgexpv (should be fast for large sparse matrices)
for (t in tvals)
	{
	Pmat = expokit_dgexpv_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
	cat("\\n\\nTime=", t, "\\n", sep="")
	print(Pmat)
	}

# DMEXPV and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
# DMEXPV is specifically designed for Markov chains and so may be slower, but more accurate.

# DMEXPV, single t-value

# DGEXPV, single t-value
expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=2)

# These functions runs the for-loop itself (sadly, we could not get mapply() to work
# on a function that calls dmexpv/dgexpv), returning a list of probability matrices.

# DGEXPV functions
list_of_P_matrices_dgexpv = expokit_wrapalldgexpv_tvals(Qmat=Qmat,
tvals=tvals, transpose_needed=TRUE)
list_of_P_matrices_dgexpv
}
\author{
  Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com} and Drew Schmidt
  \email{schmidt@math.utk.edu}
}
\seealso{
  \code{\link{expokit_dgexpv_Qmat}}
}

