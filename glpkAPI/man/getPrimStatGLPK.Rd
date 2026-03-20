\name{getPrimStatGLPK}
\alias{getPrimStatGLPK}
\alias{glp_get_prim_stat}

\title{
  Retrieve Status of Primal Basic Solution
}

\description{
  Low level interface function to the GLPK function \code{glp_get_prim_stat}.
  Consult the GLPK documentation for more detailed information.
}
\usage{
  getPrimStatGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getPrimStat} which calls the GLPK
  function \code{glp_get_prim_stat}.
}

\value{
  Status of primal basic solution
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
