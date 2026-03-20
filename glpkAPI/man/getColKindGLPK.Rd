\name{getColKindGLPK}
\alias{getColKindGLPK}
\alias{glp_get_col_kind}

\title{
  Retrieve Column Kind
}
\description{
  Low level interface function to the GLPK function \code{glp_get_col_kind}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getColKindGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number \code{j}.
  }
}

\details{
  Interface to the C function \code{getColKind} which calls the GLPK
  function \code{glp_get_col_kind}.
}

\value{
  Column Kind
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
