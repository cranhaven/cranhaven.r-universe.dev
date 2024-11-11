\name{drawable-class}
\Rdversion{1.1}
\docType{class}
\alias{drawable-class}

\title{Class \code{"drawable"}}
\description{
  Reference classes extending this virtual class must have a \code{draw} method, so their objects can be managed by \code{\link{tk.browse}} and \code{\link{browsePlot}}.
}
\author{
  Sylvain Mareschal
}
\seealso{
  \code{\linkS4class{sliceable}}, \code{\linkS4class{crossable}}
}

\keyword{classes}
\section{Extends}{
  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Custom name for the object, as a \code{character} vector of length 1.}
    \item{\code{parameters}:}{A \code{list}, storing object-specific parameters to use as \code{draw} arguments.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{callParams(chrom, start, end, ...)}:}{
      Called with draw() arguments, it returns the final argument list handling default and overloaded parameters.\cr
      - \strong{chrom, start, end, ...}   : arguments passed to draw().
    }
    \item{\code{check(warn = )}:}{
      Raises an error if the object is not valid, else returns TRUE
    }
    \item{\code{chromosomes()}:}{
      [Virtual method]\cr
      Returns the chromosome list as a vector. NULL is valid if non relevant, but should be avoided when possible.
    }
    \item{\code{defaultParams(...)}:}{
      Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.\cr
      - \strong{...}   : may be used by inheriting methods, especially for inter-dependant parameters.
    }
    \item{\code{draw(chrom, start = , end = , ...)}:}{
      [Virtual method]\cr
      Draws the object content corresponding to the defined genomic window, usually in a single plot area with coordinates in x and custom data in y.\cr
      Overloading methods should use .self$callParams(chrom, start, end ...) to handle drawing parameters and NA coordinates in a consistent way.\cr
      - \strong{chrom}   : single integer, numeric or character value, the chromosomal location.\cr
      - \strong{start}   : single integer or numeric value, inferior boundary of the window. NA should refer to 0.\cr
      - \strong{end}     : single integer or numeric value, superior boundary of the window. NA should refer to .self$getChromEnd().\cr
      - \strong{...}     : additionnal drawing parameters (precede but do not overwrite parameters stored in the object).
    }
    \item{\code{fix.param(parent = )}:}{
      Edit drawing parameters using a Tcl-tk GUI\cr
      - \strong{parent}   : tcltk parent frame for inclusion, or NULL.
    }
    \item{\code{getChromEnd(chrom)}:}{
      [Virtual method]\cr
      Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.\cr
      - \strong{chrom}   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled.
    }
    \item{\code{getName()}:}{
      'name' field accessor.
    }
    \item{\code{getParam(name, ...)}:}{
      Returns the parameter stored, or the default value if no custom value is stored for it.\cr
      - \strong{name}   : single character value, the name of the parameter to return.\cr
      - \strong{...}    : to be passed to defaultParams(), especially for inter-dependant parameters.
    }
    \item{\code{initialize(name = , parameters = , ...)}:}{}
    \item{\code{setName(value)}:}{
      'name' field mutator.
    }
    \item{\code{setParam(name, value)}:}{
      Updates a parameter stored in the object.\cr
      - \strong{name}    : single character value, the name of the parameter to set.\cr
      - \strong{value}   : the new value to assign to the parameter (any type). If missing the parameter is discarded, thus returning to dynamic default value.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callSuper (\linkS4class{envRefClass})
    \item copy (\linkS4class{envRefClass})
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item getClass (\linkS4class{envRefClass})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item show (\linkS4class{envRefClass}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
