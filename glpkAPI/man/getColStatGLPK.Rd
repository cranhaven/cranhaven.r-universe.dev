\name{getColStatGLPK}
\alias{getColStatGLPK}
\alias{glp_get_col_stat}

\title{
  Retrieve Column Status
}
\description{
  Low level interface function to the GLPK function \code{glp_get_col_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getColStatGLPK(lp, j)
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
  Interface to the C function \code{getColStat} which calls the GLPK
  function \code{glp_get_col_stat}.
}

\value{
  Column status
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
  \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
}

\keyword{ optimize }
