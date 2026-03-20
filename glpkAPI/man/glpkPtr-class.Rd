\name{glpkPtr-class}

\Rdversion{1.1}
\docType{class}

\alias{glpkPtr-class}
\alias{glpkPtr}
\alias{isGLPKpointer,glpkPtr-method}
\alias{isNULLpointerGLPK,glpkPtr-method}
\alias{isTRWKSpointer,glpkPtr-method}
\alias{glpkPointer,glpkPtr-method}
\alias{glpkPtrType,glpkPtr-method}
\alias{glpkPtrType<-,glpkPtr-method}
\alias{isGLPKpointer}
\alias{isNULLpointerGLPK}
\alias{isTRWKSpointer}
\alias{glpkPointer}
\alias{glpkPtrType}
\alias{glpkPtrType<-}

\title{Class \code{"glpkPtr"}}

\description{
  Structure of the class \code{"glpkPtr"}. Objects of that class are
  used to hold pointers to C structures used by GLPK.
}

\section{Objects from the Class}{
    Objects can be created by calls of the form\cr
    \code{test <- initProbGLPK()} or\cr
    \code{test <- mplAllocWkspGLPK()}.
}

\section{Slots}{
  \describe{
    \item{\code{glpkPtrType}:}{
        Object of class \code{"character"} giving the pointer type.
    }
    \item{\code{glpkPointer}:}{
        Object of class \code{"externalptr"} containig the pointer to a
        C structure.
    }
  }
}

\section{Methods}{
  \describe{
    \item{isGLPKpointer}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{glpkPointer(object)} is a pointer to a
        GLPK problem object, otherwise \code{FALSE}.
    }
    \item{isNULLpointerGLPK}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{glpkPointer(object)} is a NULL pointer,
        otherwise \code{FALSE}.
    }
    \item{isTRWKSpointer}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{glpkPointer(object)} is a pointer to a
        MathProg translator workspace, otherwise \code{FALSE}.
    }
    \item{glpkPointer}{\code{signature(object = "glpkPtr")}:
        gets the \code{glpkPointer} slot.
    }
    \item{glpkPtrType}{\code{signature(object = "glpkPtr")}:
        gets the \code{glpkPtrType} slot.
    }
    \item{glpkPtrType<-}{\code{signature(object = "glpkPtr")}:
        sets the \code{glpkPtrType} slot.
    }
  }
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
  \code{\link{mplAllocWkspGLPK}} and \code{\link{initProbGLPK}}.
}

\examples{
    showClass("glpkPtr")
}

\keyword{ optimize }

