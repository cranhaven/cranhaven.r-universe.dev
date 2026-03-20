\name{getRowStatGLPK}
\alias{getRowStatGLPK}
\alias{glp_get_row_stat}

\title{
  Retrieve Row Status
}

\description{
  Low level interface function to the GLPK function \code{glp_get_row_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getRowStatGLPK(lp, i)
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
  Interface to the C function \code{getRowStat} which calls the GLPK
  function \code{glp_get_row_stat}.
}

\value{
  Row status
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
