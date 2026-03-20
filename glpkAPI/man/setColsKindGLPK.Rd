\name{setColsKindGLPK}
\alias{setColsKindGLPK}

\title{
  Set Column Kind for a Set of Columns
}
\description{
  This is an advanced version of \code{\link{setColKindGLPK}}.
  Here, \code{j} can be an integer vector.
}
\usage{
  setColsKindGLPK(lp, j, kind)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    An integer vector of column indices.
  }
  \item{kind}{
    An integer vector of column kinds, for possible values see
    \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
  }
}

\details{
  Interface to the C function \code{setColsKind} which calls the GLPK
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

