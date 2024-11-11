\name{RDT storage files}
\alias{saveRDT}
\alias{readRDT}
\title{
  Single refTable object storage
}

\description{
  Functions to write a single \code{\linkS4class{refTable}} object to a file, and to restore it.
}

\usage{
  saveRDT(object, file, compress = "gzip", compression_level = 6)
  readRDT(file, version = FALSE)
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{refTable}} to store.
}
  \item{file}{
    A connection or the name of the file where the R object is saved to or read from. The '.rdt' file extension is recommended, but not mandatory.
}
  \item{compress}{
    To be passed to \code{\link{save}}
}
  \item{compression_level}{
    To be passed to \code{\link{save}}
}
  \item{version}{
    Single logical value, whether to return the stored object or the version of the package used to store it.
}
}

\details{
  These functions mimic the \code{\link{saveRDS}} and \code{\link{saveRDS}} system, without storing the class definition in the file (which can lead to about 100 useless Ko of data and longer loading times). It is intented to manage all classes extending \code{\linkS4class{refTable}}, but no guarantee is provided for classes with non-atomic slots (particularly environment-derived ones).
}

\value{
  \code{saveRDT} returns nothing, \code{readRDT} returns the object stored in the file or a single character value (depends on the \code{version} argument).
}

\author{
  Sylvain Mareschal
}

\note{
  To avoid whole-environment copying, environments of \code{function} slots are discarded.
}

\seealso{
  \code{\link{refTable-class}}, \code{\link{saveRDS}}, \code{\link{saveRDS}}
}
