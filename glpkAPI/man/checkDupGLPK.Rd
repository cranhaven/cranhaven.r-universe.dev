\name{checkDupGLPK}
\alias{checkDupGLPK}
\alias{glp_check_dup}

\title{
  Check for Duplicate Elements in Sparse Matrix
}

\description{
  Low level interface function to the GLPK function \code{glp_check_dup}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  checkDupGLPK(m, n, ne, ia, ja)
}

\arguments{
  \item{m}{
    Number of rows in the matrix.
  }
  \item{n}{
    Number of columns in the matrix.
  }
  \item{ne}{
    Number of non-zero elements in the matrix.
  }
  \item{ia}{
    Row indices of the non-zero elements.
  }
  \item{ja}{
    Column indices of the non-zero elements.
  }
}

\details{
  Interface to the C function \code{checkDup} which calls the GLPK
  function \code{glp_check_dup}.
}

\value{
 Returns one of the following values:
 \item{\code{0}  }{No duplikate elements.}
 \item{\code{-k} }{Indices \code{ia[k]} or \code{ja[k]} are out of range.}
 \item{\code{+k} }{Element \code{(ia[k], ja[k])} is duplicate.}
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }

