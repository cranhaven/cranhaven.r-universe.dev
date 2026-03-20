\name{getRowPrimIptGLPK}
\alias{getRowPrimIptGLPK}
\alias{glp_ipt_row_prim}

\title{
  Retrieve Row Primal Value
}

\description{
  Low level interface function to the GLPK function \code{glp_ipt_row_prim}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getRowPrimIptGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Row number \code{i}.
  }
}

\details{
  Interface to the C function \code{getRowPrimIpt} which calls the GLPK
  function \code{glp_ipt_row_prim}.
}

\value{
  Row primal value
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
