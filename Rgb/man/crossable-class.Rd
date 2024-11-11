\name{crossable-class}
\Rdversion{1.1}
\docType{class}
\alias{crossable-class}

\title{Class \code{"crossable"}}
\description{
  Reference classes extending this virtual class must have a \code{slice} method, as a generic \code{cross} method based on it is provided.
  
  Its only purpose is currently to add the \code{cross} method to \code{"\linkS4class{track.table}"}, as \code{"\linkS4class{sliceable}"} does not guarantee that \code{slice} returns a data.frame as \code{crossable} needs one.
}
\author{
  Sylvain Mareschal
}
\seealso{
  \code{\linkS4class{drawable}}, \code{\linkS4class{sliceable}}, \code{\linkS4class{track.table}}, \code{cghRA.array}
}

\keyword{classes}
\section{Extends}{
  Class \code{\linkS4class{sliceable}}, directly.\cr
  Class \code{\linkS4class{drawable}}, by class \code{\linkS4class{sliceable}}, distance 2.

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
    \item{\code{cross(annotation, colname = , type = , fuzziness = , maxElements = , location = , precision = , quiet = )}:}{
      Add a new column computed from overlaps with an other crossable object.\cr
      - \strong{annotation}    : other crossable object to compute overlap with.\cr
      - \strong{colname}       : single character value, the name of the new column to add to .self. If NULL or NA, the result will be returned rather than added to .self.\cr
      - \strong{type}          : single character value, either :\cr
                        'cover', to compute coverage of 'annotation' elements for each .self element\cr
                        'count', to count 'annotation' elements overlapping each .self element\cr
                        'cytoband', to get cytogenetic coordinates from a cytoband annotation track\cr
                        an 'annotation' column name, to list 'annotation' elements overlapping each .self element\cr
      - \strong{fuzziness}     : single integer value, to be added on each side of .self elements when computing overlaps.\cr
      - \strong{maxElements}   : single integer value, when more overlaps are found, lists are replaced by counts. Can be NA to disable this behavior.\cr
      - \strong{location}      : character vector, the 'chrom' / 'start' / 'end' .self columns to use for annotation.\cr
      - \strong{precision}     : single integer value from 1 to 4, amount of digits to consider in banding (type='cytoband').\cr
      - \strong{quiet}         : single logical value, wether to throw progression messages or not.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{drawable})
    \item chromosomes (\linkS4class{drawable})
    \item copy (\linkS4class{envRefClass})
    \item defaultParams (\linkS4class{sliceable})
    \item draw (\linkS4class{sliceable})
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item fix.param (\linkS4class{drawable})
    \item getChromEnd (\linkS4class{sliceable})
    \item getClass (\linkS4class{envRefClass})
    \item getName (\linkS4class{drawable})
    \item getParam (\linkS4class{drawable})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item initialize (\linkS4class{drawable})
    \item setName (\linkS4class{drawable})
    \item setParam (\linkS4class{drawable})
    \item show (\linkS4class{sliceable})
    \item slice (\linkS4class{sliceable})
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
