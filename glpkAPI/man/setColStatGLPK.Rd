\name{setColStatGLPK}
\alias{setColStatGLPK}
\alias{glp_set_col_stat}

\title{
  Set column status
}

\description{
  Low level interface function to the GLPK function \code{glp_set_col_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setColStatGLPK(lp, j, stat)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number j.
  }
  \item{stat}{
    A status parameter, see \code{\link{glpkConstants}}, section
    \sQuote{LP/MIP problem object} for possible values.
  }
}

\details{
  Interface to the C function \code{setColStat} which calls the GLPK
  function \code{glp_set_col_stat}.
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
