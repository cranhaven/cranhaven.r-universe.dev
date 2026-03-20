\name{getColsLowBndsGLPK}
\alias{getColsLowBndsGLPK}

\title{
  Retrieve Lower Bounds of Specified Columns
}

\description{
  This is an advanced version of \code{\link{getColLowBndGLPK}}.
  Here, \code{j} can be an integer vector.
}

\usage{
  getColsLowBndsGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Vector of column numbers.
  }
}

\details{
  Interface to the C function \code{getColsLowBnds} which calls the GLPK
  function \code{glp_get_col_lb}.
}

\value{
  The lower bounds of all specified columns (\code{j}) (the
  corresponding structural variables) are returned.
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

