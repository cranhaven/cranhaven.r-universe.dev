\name{getDualStatGLPK}
\alias{getDualStatGLPK}
\alias{glp_get_dual_stat}

\title{
  Retrieve Status of Dual Basic Solution
}
\description{
  Low level interface function to the GLPK function \code{glp_get_dual_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getDualStatGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getDualStat} which calls the GLPK
  function \code{glp_get_dual_stat}.
}

\value{
  Status of dual basic solution
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
