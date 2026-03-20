\name{setRowBndGLPK}
\alias{setRowBndGLPK}
\alias{glp_set_row_bnds}

\title{
  Set/Change Row Bounds
}

\description{
  Low level interface function to the GLPK function \code{glp_set_row_bnds}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setRowBndGLPK(lp, i, type, lb, ub)
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
  \item{type}{
    Row type. For possible values, see \code{\link{glpkConstants}}, section
    \sQuote{LP/MIP problem object}.
  }
  \item{lb}{
    Lower bound.
  }
  \item{ub}{
    Upper bound.
  }
}

\details{
  Interface to the C function \code{setRowBnd} which calls the GLPK
  function \code{glp_set_row_bnds}.
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


\seealso{
  \code{\link{glpkConstants}}
}

\keyword{ optimize }

