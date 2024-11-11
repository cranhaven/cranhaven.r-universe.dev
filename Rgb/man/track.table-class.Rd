\name{track.table-class}
\Rdversion{1.1}
\docType{class}
\alias{track.table-class}

\title{Class \code{"track.table"}}
\description{
  \code{"track.table"} describes a collection of features localized on a genome, defined by a chromosomal location ("chrom"), two boundaries ("start" and "end") and various data (other columns).

  Objects can be created by two distincts means :
  \itemize{
    \item{The \code{\link{track.table}} constructor, similar to the \code{data.frame} constructor. It imports a single \code{data.frame} or a collection of vectors into the object, reorder it to meet the \code{track.table} class restrictions (see below) and check immediatly for validity.}
    \item{The \code{new} function (standard behavior for S4 and reference classes), which produces an empty object and do NOT check for validity. It takes as named arguments the values to store in the fields of the various classes the new object will inherit from.}
  }
  
  3 columns are mandatory in \code{"track.table"}, with restricted names and types :
  \describe{
    \item{name}{The unique name of the feature, as \code{character}.}
    \item{chrom}{The chromosomal location of the feature, as \code{integer} or \code{factor}.}
    \item{start}{The starting position of the feature on the chromosome, as \code{integer}.}
    \item{end}{The ending position of the feature on the chromosome, as \code{integer}.}
  }
  
  The track is supposed to be ordered by chromosome, then by starting position. When chromosomes are stored as \code{factors}, they need to be numerically ordered by their internal codes (as the \code{order} function does), not alphabetically by their labels.
  
  As the \code{slice} method relies on a row index, each update in the feature coordinates must be followed by a call to the \code{buildIndex} method, a behavior that is enforced by overloading most of \code{\linkS4class{refTable}}-inherited methods.
  
  As the \code{slice} method relies on a R call object using \code{\linkS4class{refTable}} column references, each update in the column names must be followed by a call to the \code{buildCalls} method, a behavior that is enforced by overloading most of \code{\linkS4class{refTable}}-inherited methods. 
}
\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{refTable-class}}
  
  \code{\link{track.table}}, \code{\link{subtrack}}, \code{\link{istrack}}
}
\examples{
  # Exemplar data : subset of human genes
  data(hsGenes)
  
  # Construction
  trackTable <- track.table(
    hsGenes,
    .name = "NCBI Genes",
    .organism = "Homo sapiens",
    .assembly = "GRCh37"
  )
  
  # Slicing
  print(trackTable$slice(chrom="1", as.integer(15e6), as.integer(20e6)))
}

\keyword{classes}
\section{Extends}{
  Class \code{\linkS4class{refTable}}, directly.\cr
  Class \code{\linkS4class{crossable}}, directly.\cr
  Class \code{\linkS4class{sliceable}}, by class \code{\linkS4class{crossable}}, distance 2.\cr
  Class \code{\linkS4class{drawable}}, by class \code{\linkS4class{crossable}}, distance 3.

  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  \describe{
    \item{\code{assembly}:}{Single \code{character} value, the assembly version for the coordinates stored in the object. Must have length 1, should not be \code{NA}.}
    \item{\code{checktrack}:}{A \code{call} to the C external "checktrack", for faster object check (see the \code{check} method).}
    \item{\code{index}:}{\code{integer} vector, giving the index of the first row in each chromosome. See the \code{\link{subtrack}} function for further details.}
    \item{\code{organism}:}{Single \code{character} value, the name of the organism whose data is stored in the object. Must have length 1, should not be \code{NA}.}
    \item{\code{sizetrack}:}{A \code{call} to the C external "track", for faster row counting (see the \code{size} method).}
    \item{\code{subtrack}:}{A \code{call} to the C external "track", for faster slicing (see the \code{slice} method).}
  }
  The following fields are inherited (from the corresponding class):
  \itemize{
    \item colCount (\linkS4class{refTable})
    \item colIterator (\linkS4class{refTable})
    \item colNames (\linkS4class{refTable})
    \item colReferences (\linkS4class{refTable})
    \item name (\linkS4class{drawable})
    \item parameters (\linkS4class{drawable})
    \item rowCount (\linkS4class{refTable})
    \item rowNamed (\linkS4class{refTable})
    \item rowNames (\linkS4class{refTable})
    \item values (\linkS4class{refTable})
  }
}
\section{Methods}{
  \describe{
    \item{\code{addArms(centromeres, temp = )}:}{
      Adds an arm localization ('p' or 'q') to the 'chrom' column.\cr
      - \strong{centromeres}   : named numeric vector, providing the centromere position of each chromosome. Can also be a band track, as returned by track.UCSC_bands().\cr
      - \strong{temp}          : single logical value, whether to alter the object or return an altered copy.
    }
    \item{\code{buildCalls()}:}{
      Updates 'checktrack' and 'subtrack' fields. To be performed after each modification of colNames and colReferences (concerned methods are overloaded to enforce this).
    }
    \item{\code{buildGroupPosition(groupBy, colName = , reverse = )}:}{
      Adds a column to be used as 'groupPosition' by draw.boxes()\cr
      - \strong{groupBy}   : single character value, the name of a column to group rows on.\cr
      - \strong{colName}   : single character value, the name of the column to buid.\cr
      - \strong{reverse}   : single logical value, whether to reverse numbering on reverse strand or not.
    }
    \item{\code{buildGroupSize(groupBy, colName = )}:}{
      Adds a column to be used as 'groupSize' by draw.boxes()\cr
      - \strong{groupBy}   : single character value, the name of a column to group rows on.\cr
      - \strong{colName}   : single character value, the name of the column to buid.
    }
    \item{\code{buildIndex()}:}{
      Updates the 'index' parameter, should be done after any change made on the 'chrom' column (concerned methods are overloaded to enforce this).
    }
    \item{\code{eraseArms(temp = )}:}{
      Removes 'p' and 'q' added by the addArms() method from the 'chrom' column.\cr
      - \strong{temp}   : single logical value, whether to alter the object or return an altered copy.
    }
    \item{\code{isArmed()}:}{
      Detects whether the 'chrom' column refers to whole chromosomes or chromosome arms.
    }
    \item{\code{segMerge(...)}:}{
      Apply the segMerge() function to the track content.\cr
      - \strong{...}   : arguments to be passed to segMerge().
    }
    \item{\code{segOverlap(...)}:}{
      Apply the segOverlap() function to the track content.\cr
      - \strong{...}   : arguments to be passed to segOverlap().
    }
    \item{\code{size(chrom, start, end)}:}{
      Count elements in the specified window.\cr
      - \strong{chrom}   : single integer, numeric or character value, the chromosomal location.\cr
      - \strong{start}   : single integer or numeric value, inferior boundary of the window.\cr
      - \strong{end}     : single integer or numeric value, superior boundary of the window.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item addColumn (\linkS4class{refTable}, overloaded)
    \item addDataFrame (\linkS4class{refTable})
    \item addEmptyRows (\linkS4class{refTable})
    \item addList (\linkS4class{refTable}, overloaded)
    \item addVectors (\linkS4class{refTable})
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{refTable}, overloaded)
    \item chromosomes (\linkS4class{drawable}, overloaded)
    \item coerce (\linkS4class{refTable}, overloaded)
    \item colOrder (\linkS4class{refTable})
    \item copy (\linkS4class{refTable})
    \item cross (\linkS4class{crossable})
    \item defaultParams (\linkS4class{sliceable}, overloaded)
    \item delColumns (\linkS4class{refTable}, overloaded)
    \item draw (\linkS4class{sliceable})
    \item erase (\linkS4class{refTable})
    \item export (\linkS4class{envRefClass})
    \item extract (\linkS4class{refTable})
    \item field (\linkS4class{envRefClass})
    \item fill (\linkS4class{refTable}, overloaded)
    \item fix.param (\linkS4class{drawable})
    \item getChromEnd (\linkS4class{sliceable}, overloaded)
    \item getClass (\linkS4class{envRefClass})
    \item getColCount (\linkS4class{refTable})
    \item getColNames (\linkS4class{refTable})
    \item getLevels (\linkS4class{refTable})
    \item getName (\linkS4class{drawable})
    \item getParam (\linkS4class{drawable})
    \item getRefClass (\linkS4class{envRefClass})
    \item getRowCount (\linkS4class{refTable})
    \item getRowNames (\linkS4class{refTable})
    \item import (\linkS4class{envRefClass})
    \item indexes (\linkS4class{refTable})
    \item initFields (\linkS4class{envRefClass})
    \item initialize (\linkS4class{refTable}, overloaded)
    \item metaFields (\linkS4class{refTable}, overloaded)
    \item rowOrder (\linkS4class{refTable}, overloaded)
    \item setColNames (\linkS4class{refTable}, overloaded)
    \item setLevels (\linkS4class{refTable}, overloaded)
    \item setName (\linkS4class{drawable})
    \item setParam (\linkS4class{drawable})
    \item setRowNames (\linkS4class{refTable})
    \item show (\linkS4class{refTable}, overloaded)
    \item slice (\linkS4class{sliceable}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item types (\linkS4class{refTable})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
