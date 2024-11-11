\name{segOverlap}
\alias{segOverlap}
\title{
  Merges overlapping segments
}

\description{
  Given a set of segments defined by "chrom", "start", "end" and various data, it merges overlapping or jointive rows. 
}

\usage{
  segOverlap(segTable, fun = list(unique.default, start=min, end=max),
    factorsAsIntegers = TRUE)
}

\arguments{
  \item{segTable}{
    A \code{data.frame} of segments, with at least "chrom", "start" and "end" columns. Standard behavior (default \code{fun} value) assumes "start" and "end" to be at least \code{numeric}, preferably \code{integer}.
}
  \item{fun}{
    A \code{list} of \code{function}s, defining how to merge values when merging rows. It should contain an unamed element for the default function, and named elements to deal with specific columns.
}
  \item{factorsAsIntegers}{
    Single logical value, whether to handle columns of class factor as integers or as is. Using \code{TRUE} dramatically shortens computation time in default user case ("chrom" column of class factor, "unique" function applied), but \code{FALSE} may be required to allow non-standard merging functions to access encoded values rather than factor indexes.
}
}

\value{
  Returns a \code{data.frame} similar to \code{segTable}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{segMerge}}
}

