\name{setColsNamesGLPK}
\alias{setColsNamesGLPK}

\title{
  Set/Change Column Names
}

\description{
  This is an advanced version of \code{\link{setColNameGLPK}}.
  Here, \code{j} can be an integer vector, \code{cnames} can be a character
  vector.
}
\usage{
  setColsNamesGLPK(lp, j, cnames = NULL)
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
  \item{cnames}{
    Vector of column names of the same length as \code{j} or \code{NULL}.
  }
}

\details{
  Interface to the C function \code{setColsNames} which calls the GLPK
  function \code{glp_set_col_name}.

  If \code{cnames} is set to \code{NULL}, all column names for column indices in
  \code{j} will be removed. If \code{cname[k]} is the empty string \code{""},
  column name \code{j[k]} will be removed.
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

