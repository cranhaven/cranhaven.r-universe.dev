\name{getRowsTypesGLPK}
\alias{getRowsTypesGLPK}

\title{
  Retrieve Types of Specified Constraints (Rows)
}

\description{
  This is an advanced version of \code{\link{getRowTypeGLPK}}.
  Here, \code{i} can be an integer vector.
}

\usage{
  getRowsTypesGLPK(lp, i)
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
  Interface to the C function \code{getRowsTypes} which calls the GLPK
  function \code{glp_get_row_type}.
}

\value{
  A numeric vector of the same length as \code{i} giving the constraint type
  of the specified rows.
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
  \code{\link{glpkConstants}}, section
  \sQuote{type of auxiliary/structural variable}.
}

\keyword{ optimize }
