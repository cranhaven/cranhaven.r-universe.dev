\name{addRowsGLPK}
\alias{addRowsGLPK}
\alias{glp_add_rows}

\title{
  Add Rows to a GLPK Problem Object
}

\description{
  Low level interface function to the GLPK function \code{glp_add_rows}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  addRowsGLPK(lp, nrows)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{nrows}{
    The number of rows to add.
  }
}

\details{
  Interface to the C function \code{addRows} which calls the GLPK
  function \code{glp_add_rows}.
}

\value{
 The ordinal number of the first new row added to the
 problem object is returned.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }

