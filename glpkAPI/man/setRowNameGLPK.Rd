\name{setRowNameGLPK}
\alias{setRowNameGLPK}
\alias{glp_set_row_name}

\title{
  Set/Change Row Name
}

\description{
  Low level interface function to the GLPK function \code{glp_set_row_name}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setRowNameGLPK(lp, i, rname = NULL)
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
  \item{rname}{
    Row name.
  }
}

\details{
  Interface to the C function \code{setRowName} which calls the GLPK
  function \code{glp_set_row_name}.
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

