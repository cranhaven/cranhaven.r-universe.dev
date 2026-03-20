\name{setRowsNamesGLPK}
\alias{setRowsNamesGLPK}

\title{
  Set/Change Row Names
}

\description{
  This is an advanced version of \code{\link{setRowNameGLPK}}.
  Here, \code{i} can be an integer vector, \code{rnames} can be a character
  vector.
}

\usage{
  setRowsNamesGLPK(lp, i, rnames = NULL)
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
  \item{rnames}{
    Vector of row names of the same length as \code{i} or \code{NULL}.
  }
}

\details{
  Interface to the C function \code{setRowsNames} which calls the GLPK
  function \code{glp_set_row_name}.

  If \code{rnames} is set to \code{NULL}, all row names for row indices in
  \code{i} will be removed. If \code{rname[k]} is the empty string \code{""},
  row name \code{i[k]} will be removed.
}

\value{
  NULL
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

