\name{delColsGLPK}
\alias{delColsGLPK}
\alias{glp_del_cols}

\title{
  Delete Columns from Problem Object
}

\description{
  Low level interface function to the GLPK function \code{glp_del_cols}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  delColsGLPK(lp, ncols, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{ncols}{
    Number of columns to delete.
  }
  \item{j}{
    Ordinal numbers of columns to delete.
  }
}

\details{
  Interface to the C function \code{delCols} which calls the GLPK
  function \code{glp_del_cols}.
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
