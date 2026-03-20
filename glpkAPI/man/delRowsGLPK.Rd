\name{delRowsGLPK}
\alias{delRowsGLPK}
\alias{glp_del_rows}

\title{
  Delete Rows from Problem Object
}

\description{
  Low level interface function to the GLPK function \code{glp_del_rows}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  delRowsGLPK(lp, nrows, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{nrows}{
    Number of rows to delete.
  }
  \item{i}{
    Ordinal numbers of rows to delete.
  }
}

\details{
  Interface to the C function \code{delRows} which calls the GLPK
  function \code{glp_del_rows}.
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
