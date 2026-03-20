\name{getColsStatGLPK}
\alias{getColsStatGLPK}

\title{
  Retrieve Column Status of all Columns
}

\description{
  This is an advanced version of \code{\link{getColStatGLPK}}.
}

\usage{
  getColsStatGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getColsStat} which calls the GLPK
  function \code{glp_get_col_stat}.
}

\value{
  The column status of all columns are returned.
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

