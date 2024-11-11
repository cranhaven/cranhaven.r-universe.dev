\name{sliceable-class}
\Rdversion{1.1}
\docType{class}
\alias{sliceable-class}

\title{Class \code{"sliceable"}}
\description{
  Reference classes extending this virtual class must have a \code{slice} method, as a generic \code{draw} method based on it is provided.
}
\author{
  Sylvain Mareschal
}
\seealso{
  \code{\linkS4class{drawable}}, \code{\linkS4class{crossable}}, \code{cghRA.array}
}

\keyword{classes}
\section{Extends}{
  Class \code{\linkS4class{drawable}}, directly.

  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  The following fields are inherited (from the corresponding class):
  \itemize{
    \item name (\linkS4class{drawable})
    \item parameters (\linkS4class{drawable})
  }
}
\section{Methods}{
  \describe{
    \item{\code{slice(chrom, start, end)}:}{
      [Virtual method]\cr
      Extract elements in the specified window, in a format suitable to draw().\cr
      - \strong{chrom}   : single integer, numeric or character value, the chromosomal location. NA is not handled.\cr
      - \strong{start}   : single integer or numeric value, inferior boundary of the window. NA is not handled.\cr
      - \strong{end}     : single integer or numeric value, superior boundary of the window. NA is not handled.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{drawable})
    \item chromosomes (\linkS4class{drawable})
    \item copy (\linkS4class{envRefClass})
    \item defaultParams (\linkS4class{drawable}, overloaded)
    \item draw (\linkS4class{drawable}, overloaded)
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item fix.param (\linkS4class{drawable})
    \item getChromEnd (\linkS4class{drawable}, overloaded)
    \item getClass (\linkS4class{envRefClass})
    \item getName (\linkS4class{drawable})
    \item getParam (\linkS4class{drawable})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item initialize (\linkS4class{drawable})
    \item setName (\linkS4class{drawable})
    \item setParam (\linkS4class{drawable})
    \item show (\linkS4class{drawable}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
