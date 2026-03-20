\name{printRangesGLPK}
\alias{printRangesGLPK}
\alias{glp_print_ranges}

\title{
  Print Sensitivity Analysis Report
}

\description{
  Low level interface function to the GLPK function \code{glp_print_ranges}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  printRangesGLPK(lp, numrc = 0, rowcol = NULL, fname = "sar.txt")
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{numrc}{
    Length of the row/column list (argument \code{rowcol}).
  }
  \item{rowcol}{
    Ordinal numbers of rows and columns to be analyzed.
  }
  \item{fname}{
    A filename.
  }
}

\details{
  Interface to the C function \code{printRanges} which calls the GLPK
  function \code{glp_print_ranges}.
}

\value{
  Zero on success, otherwise non-zero.
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
