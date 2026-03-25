\name{SparseM_coo_to_REXPOKIT_coo}
\alias{SparseM_coo_to_REXPOKIT_coo}
\title{Convert a SparseM COO matrix to a plain matrix}
\usage{
  SparseM_coo_to_REXPOKIT_coo(tmpmat_in_SparseMcoo_fmt)
}
\arguments{
  \item{tmpmat_in_SparseMcoo_fmt}{A square matrix S4 object
  derived from SparseM's as.matrix.coo}
}
\value{
  tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia},
  \code{ja}, and \code{a}
}
\description{
  Converts a SparseM COO-formatted matrix (an S4 object) to
  a plain matrix, with \cr column #1 = ia = i index\cr
  column #2 = ja = j index\cr column #3 = a = nonzero
  values of the matrix\cr
}
\details{
  Background: COO (coordinated list) format, is described
  here:\cr

  \url{https://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr

  In \code{EXPOKIT} and its wrapper functions, a
  COO-formated matrix is input as 3 vectors (first two
  integer, the third double):\cr

  ia = row number\cr ja = column number\cr a = value of
  that cell in the matrix (skipping 0 cells)\cr
}
\examples{
# Example use:
# Make a Q matrix
tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168,
0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)

# Covert to SparseM coo format
tmpmat_in_REXPOKIT_coo_fmt <- mat2coo(tmpmat)
}
\author{
  Nicholas J. Matzke \email{matzke@berkeley.edu}
}
\seealso{
  \code{\link{mat2coo_forloop}}
}

