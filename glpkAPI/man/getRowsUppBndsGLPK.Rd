\name{getRowsUppBndsGLPK}
\alias{getRowsUppBndsGLPK}

\title{
  Retrieve Upper Bounds of Specified Rows
}

\description{
  This is an advanced version of \code{\link{getRowUppBndGLPK}}.
  Here, \code{i} can be an integer vector.
}

\usage{
  getRowsUppBndsGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Vector of row numbers.
  }
}

\details{
  Interface to the C function \code{getRowsUppBnds} which calls the GLPK
  function \code{glp_get_row_ub}.
}

\value{
  The upper bounds of all specified columns (\code{i}) (the
  corresponding auxiliary variables) are returned.
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

