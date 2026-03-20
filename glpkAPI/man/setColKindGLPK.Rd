\name{setColKindGLPK}
\alias{setColKindGLPK}
\alias{glp_set_col_kind}

\title{
  Set Column Kind
}

\description{
  Low level interface function to the GLPK function \code{glp_set_col_kind}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setColKindGLPK(lp, j, kind)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number \code{j}.
  }
  \item{kind}{
    Kind of column number \code{j}, for possible values see
    \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
  }
}

\details{
  Interface to the C function \code{setColKind} which calls the GLPK
  function \code{glp_set_col_kind}.
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

