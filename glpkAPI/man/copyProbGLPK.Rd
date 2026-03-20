\name{copyProbGLPK}
\alias{copyProbGLPK}
\alias{glp_copy_prob}

\title{
  Copy problem object content
}

\description{
  Low level interface function to the GLPK function \code{glp_copy_prob}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  copyProbGLPK(lp, clp, name = GLP_OFF)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{clp}{
    A pointer to a GLPK problem object (destination).
  }
  \item{name}{
    If set to \code{GLP_ON}, the routine copies all symbolic names;
    otherwise (\code{GLP_OFF}) not.
  }
}

\details{
  Interface to the C function \code{copyProb} which calls the GLPK
  function \code{glp_copy_prob}.
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
  \code{\link{glpkConstants}}, section \sQuote{enable/disable flag}.
}

\keyword{ optimize }
