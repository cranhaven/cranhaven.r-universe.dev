\name{mat2coo_forloop}
\alias{mat2coo_forloop}
\title{Convert matrix to COO format using nested for-loops}
\usage{
  mat2coo_forloop(tmpmat)
}
\arguments{
  \item{tmpmat}{A square matrix}
}
\value{
  tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia},
  \code{ja}, and \code{a}
}
\description{
  Converts a matrix to COO format. This version of the
  function uses for-loops, which is presumably less
  efficient than \code{\link{mat2coo}}.
}
\examples{
# Example use:
# Make a Q matrix
tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168,
0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)

# Convert to REXPOKIT coo format
tmpmat_in_REXPOKIT_coo_fmt = mat2coo_forloop(tmpmat)
tmpmat_in_REXPOKIT_coo_fmt
}
\author{
  Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com}
}
\seealso{
  \code{\link{mat2coo}}
}

