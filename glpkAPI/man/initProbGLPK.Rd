\name{initProbGLPK}
\alias{initProbGLPK}
\alias{glp_create_prob}

\title{
  Create a GLPK Problem Object
}

\description{
  Low level interface function to the GLPK function \code{glp_create_prob}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  initProbGLPK(ptrtype = "glpk_prob")
}

\arguments{
  \item{ptrtype}{
    A name for the pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{initProb} which calls the GLPK
  function \code{glp_create_prob}.
}

\value{
  An instance of class \code{"\linkS4class{glpkPtr}"}.
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
  \code{"\linkS4class{glpkPtr}"}.
}

\keyword{ optimize }

