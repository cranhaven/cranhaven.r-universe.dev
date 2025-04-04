\name{twobit_roundtrip}

\alias{twobit_read}
\alias{twobit_write}

\title{Read/write a .2bit file}

\description{
  Read/write a character vector representing DNA sequences from/to
  a file in \emph{2bit} format.
}

\usage{
twobit_read(filepath)

twobit_write(x, filepath, use.long=FALSE, skip.dups=FALSE)
}

\arguments{
  \item{filepath}{
    A single string (character vector of length 1) containing a path
    to the file to read or write.
  }
  \item{x}{
    A named character vector representing DNA sequences. The names on
    the vector should be unique and the sequences should only contain
    A's, C's, G's, T's, or N's, in uppercase or lowercase.
  }
  \item{use.long}{
    By default the \emph{2bit} format cannot store more than 4Gb of sequence
    data in total. Set \code{use.long} to \code{TRUE} if your sequence data
    is bigger than that.
  }
  \item{skip.dups}{
    By default duplicate sequence names are an error. By setting
    \code{skip.dups} to \code{FALSE}, sequences with a duplicated name will
    be skipped with a warning.
  }
}

\value{
  For \code{twobit_read()}: A named character vector containing the DNA
  sequences loaded from the file.

  For \code{twobit_write()}: \code{filepath} returned invisibly.
}

\references{
  A quick overview of the \emph{2bit} format:
  \url{https://genome.ucsc.edu/FAQ/FAQformat.html#format7}
}

\seealso{
  \code{\link{twobit_seqstats}} and \code{\link{twobit_seqlengths}} to
  extract the sequence lengths and letter counts from a \code{.2bit} file.
}

\examples{
## Read:
inpath <- system.file(package="Rtwobitlib", "extdata", "sacCer2.2bit")
dna <- twobit_read(inpath)
names(dna)
nchar(dna)

## Write:
outpath <- twobit_write(dna, tempfile())

## Sanity checks:
library(tools)
stopifnot(md5sum(inpath) == md5sum(outpath))
stopifnot(identical(nchar(dna), twobit_seqlengths(inpath)))
}

\keyword{manip}
