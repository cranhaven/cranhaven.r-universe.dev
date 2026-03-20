\name{mplPostsolveGLPK}
\alias{mplPostsolveGLPK}
\alias{glp_mpl_postsolve}

\title{
  Postsolve Model
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_postsolve}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplPostsolveGLPK(wk, lp, sol)
}

\arguments{
  \item{wk}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{mplAllocWkspGLPK}}. This is basically a pointer to a GLPK
    translocator workspace.
  }
  \item{lp}{
    A pointer to a GLPK problem object.
  }
  \item{sol}{
    Type of solution to be copied to the translator workspace, for possible
    values, see \code{\link{glpkConstants}}, section
    \sQuote{LP/MIP problem object}.
  }
}

\details{
  Interface to the C function \code{mplPostsolve} which calls the GLPK
  function \code{glp_mpl_postsolve}.
}

\value{
  Returns zero on success, otherwise it returns non-zero.
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
  \code{\link{mplAllocWkspGLPK}},
  \code{\link{mplBuildProbGLPK}},
  \code{\link{mplFreeWkspGLPK}},
  \code{\link{mplGenerateGLPK}},
  \code{\link{mplReadDataGLPK}} and
  \code{\link{mplReadModelGLPK}}.
}

\keyword{ optimize }

