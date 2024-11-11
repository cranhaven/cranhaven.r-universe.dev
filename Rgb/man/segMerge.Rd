\name{segMerge}
\alias{segMerge}
\title{
  Merges consecutive segments
}

\description{
  Given a set of segments defined by "chrom", "start", "end" and various data, it merges consecutive rows (sorted by "chrom" then "start") that share same data. As an example, it is useful to merge consecutive regions of the genome sharing same copy numbers after modelization, or filling small gaps. 
}

\usage{
  segMerge(segTable, on = names(segTable), fun = list(unique, start=min, end=max),
    group = NULL)
}

\arguments{
  \item{segTable}{
    A \code{data.frame} of segments, with at least "chrom", "start" and "end" columns. Standard behavior (default \code{fun} value) assumes "start" and "end" to be at least \code{numeric}, preferably \code{integer}.
}
  \item{on}{
    Character vector, \code{segTable} columns that must all be identical for the consecutive rows to be merged. For convenience, "chrom" is forced in and "start" / "end" are forced out.
}
  \item{fun}{
    A \code{list} of \code{function}s, defining how to merge values when merging rows. It should contain an unamed element for the default function, and named elements to deal with specific columns.
}
  \item{group}{
    A vector with as many values as \code{segTable} rows. If provided, \code{on} is ignored and \code{group} is directly used as a grouping factor to merge consecutive rows.
}
}

\value{
  Returns a \code{data.frame} similar to \code{segTable}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{segOverlap}}
}

