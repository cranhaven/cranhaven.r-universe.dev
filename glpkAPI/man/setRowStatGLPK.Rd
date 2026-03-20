\name{setRowStatGLPK}
\alias{setRowStatGLPK}
\alias{glp_set_row_stat}

\title{
  Set row status
}

\description{
  Low level interface function to the GLPK function \code{glp_set_row_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setRowStatGLPK(lp, i, stat)
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
  \item{stat}{
    A status parameter, see \code{\link{glpkConstants}} for possible values.
  }
}

\details{
  Interface to the C function \code{setRowStat} which calls the GLPK
  function \code{glp_set_row_stat}, section \sQuote{LP/MIP problem object}.
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
