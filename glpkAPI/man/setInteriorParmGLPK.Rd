\name{setInteriorParmGLPK}
\alias{setInteriorParmGLPK}

\title{
  Sets/Changes Control Parameters or the Interior-point Method.
}

\description{
  Sets/Changes the values of corresponding members of in the structure
  \code{glp_iptcp}. Consult the GLPK documentation for more detailed
  information.
}

\usage{
  setInteriorParmGLPK(parm, val)
}

\arguments{
  \item{parm}{
    A vector containing integer values or symbolic names
    of the control parameters to be changed
    (see \code{\link{glpkConstants}}, section \sQuote{Control Parameters}) and
    \sQuote{interior-point solver control parameters}).
  }
  \item{val}{
    A vector containing the new values for the corresponding
    control parameters.
  }
}

\details{
  The Arguments \code{parm} and \code{val} must have the
  same length. The value \code{val[i]} belongs to the
  parameter \code{parm[i]}.
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
  \code{\link{glpkConstants}}
}

\keyword{ optimize }

