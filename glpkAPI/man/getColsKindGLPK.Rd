\name{getColsKindGLPK}
\alias{getColsKindGLPK}

\title{
  Retrieve Column Kind
}

\description{
  This is an advanced version of \code{\link{getColKindGLPK}}.
}

\usage{
  getColsKindGLPK(lp, j)
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
  Interface to the C function \code{getColsKind} which calls the GLPK
  function \code{glp_get_col_ub}.
}

\value{
  The column kinds of all specified columns (\code{j}) are returned.
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

