\name{getMatColGLPK}
\alias{getMatColGLPK}
\alias{glp_get_mat_col}

\title{
  Retrieves Column j of the Constraint Matrix.
}

\description{
  Low level interface function to the GLPK function \code{glp_get_mat_col}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getMatColGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number j.
  }
}

\details{
  Interface to the C function \code{getMatCol} which calls the GLPK
  functions \code{glp_get_num_rows} and \code{glp_get_mat_col}.
}

\value{
  Returns NULL or a list containing the non zero elements of column j:
  \item{nnz}{number of non zero elements in column j}
  \item{index}{row indices of the non zero elements in column j}
  \item{value}{numerical values of the non zero elements in column j}
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

