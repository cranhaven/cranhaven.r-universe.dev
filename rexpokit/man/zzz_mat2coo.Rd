\name{mat2coo}
\alias{mat2coo}
\title{Convert matrix to COO format using SparseM function}
\usage{
  mat2coo(tmpmat)
}
\arguments{
  \item{tmpmat}{A square matrix}
}
\value{
  tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia},
  \code{ja}, and \code{a}
}
\description{
  Converts a matrix to COO format using the SparseM
  function, presumably this is faster than using a
  for-loop.\cr
}
\details{
  \code{EXPOKIT}'s \code{dmexp}-type functions deal with
  sparse matrices. These have a lot of zeros, and thus can
  be compressed into COO (coordinated list) format, which
  is described here:\cr

  \url{https://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr

  In \code{EXPOKIT} and its wrapper functions, a
  COO-formated matrix is input as 3 vectors (first two
  integer, the third double):\cr

  ia = row number\cr ja = column number\cr a = value of
  that cell in the matrix (skipping 0 cells)\cr
}
\examples{
# Example use:
}
\author{
  Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com}
}
\seealso{
  \code{\link{mat2coo_forloop}}
}

