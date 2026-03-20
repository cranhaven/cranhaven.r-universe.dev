\name{getMatRowGLPK}
\alias{getMatRowGLPK}
\alias{glp_get_mat_row}

\title{
  Retrieves Row i of the Constraint Matrix.
}

\description{
  Low level interface function to the GLPK function \code{glp_get_mat_row}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getMatRowGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Row number i.
  }
}

\details{
  Interface to the C function \code{getMatRow} which calls the GLPK
  functions \code{glp_get_num_cols} and \code{glp_get_mat_row}.
}

\value{
  Returns NULL or a list containing the non zero elements of row i:
  \item{nnz}{number of non zero elements in row i}
  \item{index}{column indices of the non zero elements in row i}
  \item{value}{numerical values of the non zero elements in row i}
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }

