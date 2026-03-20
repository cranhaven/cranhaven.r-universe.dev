\name{mplReadModelGLPK}
\alias{mplReadModelGLPK}
\alias{glp_mpl_read_model}

\title{
  Read and Translate Model Section
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_read_model}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplReadModelGLPK(wk, fname, skip)
}

\arguments{
  \item{wk}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{mplAllocWkspGLPK}}. This is basically a pointer to a GLPK
    translocator workspace.
  }
  \item{fname}{
    The name of the model file to be read in.
  }
  \item{skip}{
    Flag, how to treat the data section.
  }
}

\details{
  Interface to the C function \code{mplReadModel} which calls the GLPK
  function \code{glp_mpl_read_model}.
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
  \code{\link{mplReadDataGLPK}}.
}

\keyword{ optimize }

