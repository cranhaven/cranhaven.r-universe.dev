\name{subtrack}
\alias{subtrack}
\alias{sizetrack}
\alias{istrack}
\title{
  Extract elements within a genomic window
}

\description{
  \code{subtrack} extracts lines from a \code{data.frame}, \code{list} or \code{vector} collection within a single genomic window, defined by a chromosome name, a starting and an ending positions. As this is a common task in genome-wide analysis, this function relies on an optimized C code in order to achieve good performances.

  \code{sizetrack} is very similar to \code{subtrack}, but only count lines without extracting the data.
  
  \code{istrack} checks if a collection of data is suitable for \code{subtrack} and \code{sizetrack} (See 'Track definition' for further details). As this operation is quite expensive and should be performed once, it is up to the user to check its data before subtracking.
}

\usage{
  istrack(...)
  subtrack(...)
  sizetrack(...)
}

\arguments{
  \item{\dots}{
    A collection of data to be considered as a single track. Named vectors are considered as single columns, \code{data.frame} and \code{list} as collections of columns, all parallelized in a single bidimensional table (assuming they all have same lengths / row counts). See 'Track definition' for further details.
    
    For \code{subtrack} and \code{sizetrack}, the first arguments must be the following (preferably unnamed) :
    \itemize{
      \item{chromosome location (\code{integer} or \code{character}, according to 'chrom' type)}
      \item{starting position on the chromosome (\code{integer}, considered within)}
      \item{ending position on the chromosome (\code{integer}, considered within)}
      \item{chromosome index (\code{integer} vector, see below)}
    }
}
}

\section{Track definition}{
  A track is defined as a \code{data.frame} with a variable amount of data (in columns) about a variable amount of features (in rows).
  
  3 columns are mandatory, with restricted names and types :
  \describe{
    \item{chrom}{The chromosomal location of the feature, as \code{integer} or \code{factor}.}
    \item{start}{The starting position of the feature on the chromosome, as \code{integer}.}
    \item{end}{The ending position of the feature on the chromosome, as \code{integer}.}
  }
  
  The track is supposed to be ordered by chromosome, then by starting position. When chromosomes are stored as \code{factors}, they need to be numerically ordered by their internal codes (as the \code{order} function does), not alphabetically by their labels.
}

\section{Chromosome index}{
  In order to guarantee good performances, chromosomes are to be indexed. As the rows are supposed to be ordered by chromosome, then by starting position (see 'Track definition'), reminding starting or ending rows of each chromosome can save huge amounts of computation time in large tracks.
  
  The following specifications must be fulfilled :
  \itemize{
    \item{It must be an \code{integer} vector, with the last row index of each chromosome in the track indexed.}
    \item{Values are to be ordered by chromosome, in the same way than the 'chrom' column.}
    \item{For \code{integer} 'chrom', values are extracted by position (chromosome '1' is the first value ...).}
    \item{For \code{factor} 'chrom', values are extracted by names (named with 'chrom' levels).}
    \item{Chromosomes without data in the track must be described, with \code{NA integer} values.}
  }
  
  See the 'Example' section below for index computation.
}

\details{
  The C code relies heavily on the ordering to fastly retrieve the elements that overlap the queried window. Elements entirely comprised in the window are returned, as well as elements that only partially overlap it.
}

\value{
  \code{subtrack} returns a single \code{data.frame} merging all columns provided, with the subset of rows corresponding to elements in the queried window. This \code{data.frame} has no row name, and is a valid track (See 'Track definition' for further details).

  \code{sizetrack} returns a single \code{integer} value corresponding to the count of rows in the queried window.
  
  \code{istrack} returns a single \code{TRUE} value if the data collection provided is a valid track. Otherwise it returns a single \code{FALSE} value, with a "why" attribute containing a single character string explaining the (first) condition that is not fulfilled.
}

\note{
  These three functions are proposed for generic usage on \code{data.frame}, \code{list} or vectors. The \code{\linkS4class{track.table}} class implements more suitable \code{slice}, \code{size} and \code{check} methods, and handles autonomously the indexing.
}

\author{
  Sylvain Mareschal
}

\examples{
  
  # Exemplar data : subset of human genes
  data(hsGenes)
  
  # Track validity
  print(istrack(hsGenes))
  hsGenes <- hsGenes[ order(hsGenes$chrom, hsGenes$start) ,]
  print(istrack(hsGenes))
  
  # Chromosome index (factorial 'chrom')
  index <- tapply(1:nrow(hsGenes), hsGenes$chrom, max)
  
  # Factor chrom query
  print(class(hsGenes$chrom))
  subtrack("1", 10e6, 15e6, index, hsGenes)
  
  # Row count
  a <- nrow(subtrack("1", 10e6, 15e6, index, hsGenes))
  b <- sizetrack("1", 10e6, 15e6, index, hsGenes)
  if(a != b) stop("Inconsistency")
  
  # Multiple sources
  length <- hsGenes$end - hsGenes$start
  subtrack("1", 10e6, 15e6, index, hsGenes, length)
  subtrack("1", 10e6, 15e6, index, hsGenes, length=length)
  
  # Speed comparison (x200 here)
  system.time(
    for(i in 1:40000) {
      subtrack("1", 10e6, 15e6, index, hsGenes)
    }
  )
  system.time(
    for(i in 1:200) {
      hsGenes[ hsGenes$chrom == "1" & hsGenes$start <= 15e6 & hsGenes$end >= 10e6 ,]
    }
  )
  
  # Convert chrom from factor to integer
  hsGenes$chrom <- as.integer(as.character(hsGenes$chrom))
  
  # Chromosome index (integer 'chrom')
  index <- rep(NA_integer_, 24)
  tmpIndex <- tapply(1:nrow(hsGenes), hsGenes$chrom, max)
  index[ as.integer(names(tmpIndex)) ] <- tmpIndex
  
  # Integer chrom query
  print(class(hsGenes$chrom))
  subtrack(1, 10e6, 15e6, index, hsGenes)
}
