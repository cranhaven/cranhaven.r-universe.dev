\name{setProbNameGLPK}
\alias{setProbNameGLPK}
\alias{glp_set_prob_name}

\title{
  Set/Change Problem Name
}

\description{
  Low level interface function to the GLPK function \code{glp_set_prob_name}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setProbNameGLPK(lp, pname = NULL)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{pname}{
    Problem name.
  }
}

\details{
  Interface to the C function \code{setProbName} which calls the GLPK
  function \code{glp_set_prob_name}.
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


\keyword{ optimize }

