\name{mplReadDataGLPK}
\alias{mplReadDataGLPK}
\alias{glp_mpl_read_data}

\title{
  Read and Translate Data Section
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_read_data}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplReadDataGLPK(wk, fname)
}

\arguments{
  \item{wk}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{mplAllocWkspGLPK}}. This is basically a pointer to a GLPK
    translocator workspace.
  }
  \item{fname}{
    The name of the data file to be read in.
  }
}

\details{
  Interface to the C function \code{mplReadData} which calls the GLPK
  function \code{glp_mpl_read_data}.
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
  \code{\link{mplPostsolveGLPK}} and
  \code{\link{mplReadModelGLPK}}.
}

\keyword{ optimize }

