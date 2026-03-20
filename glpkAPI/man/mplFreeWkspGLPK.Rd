\name{mplFreeWkspGLPK}
\alias{mplFreeWkspGLPK}
\alias{glp_mpl_free_wksp}

\title{
  Free Translator Workspace
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_free_wksp}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplFreeWkspGLPK(wk)
}

\arguments{
  \item{wk}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{mplAllocWkspGLPK}}. This is basically a pointer to a GLPK
    translocator workspace.
  }
}

\details{
  Interface to the C function \code{mplFreeWksp} which calls the GLPK
  function \code{glp_mpl_free_wksp}.
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
  \code{\link{mplAllocWkspGLPK}},
  \code{\link{mplBuildProbGLPK}},
  \code{\link{mplGenerateGLPK}},
  \code{\link{mplPostsolveGLPK}},
  \code{\link{mplReadDataGLPK}} and
  \code{\link{mplReadModelGLPK}}.
}

\keyword{ optimize }

