\name{coo2mat}
\alias{coo2mat}
\title{Convert a COO-formated matrix to standard square format}
\usage{
  coo2mat(coomat,
    n = max(max(coomat[, 1]), max(coomat[, 2])),
    transpose_needed = FALSE)
}
\arguments{
  \item{coomat}{a 3-column matrix or data.frame (basically
  \code{cbind(ia, ja, a)})}

  \item{n}{the order of the matrix}

  \item{transpose_needed}{If TRUE (default), matrix will be
  transposed (apparently EXPOKIT needs the input matrix to
  be transposed compared to normal)}
}
\value{
  outmat
}
\description{
  \code{EXPOKIT}'s \code{dmexp}-type functions deal with
  sparse matrices. These have a lot of zeros, and thus can
  be compressed into COO (coordinated list) format, which
  is described here:\cr
}
\details{
  \url{https://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr

  In \code{EXPOKIT} and its wrapper functions, a
  COO-formated matrix is input as 3 vectors (first two
  integer, the third double):\cr

  ia = row number\cr ja = column number\cr a = value of
  that cell in the matrix (skipping 0 cells)\cr

  This function takes a 3-column matrix or data.frame
  (basically \code{cbind(ia, ja, a)}) and the order of the
  matrix, \code{n} (n = the order of the matrix, i.e.
  number of rows/columns) and converts back to standard
  square format.\cr
}
\examples{
# Example use:
ia = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
ja = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
a  = c(-1.218, 0.126, 0.168, 0.126, 0.504, -0.882, 0.504,
0.672, 0.336, 0.252, -1.050, 0.252, 0.378, 0.504, 0.378, -1.050)
coomat = cbind(ia, ja, a)
print(coomat)
n = 4
Qmat = coo2mat(coomat, n)
print(Qmat)
}
\author{
  Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com}
}


