\name{twobit_seqstats}

\alias{twobit_seqstats}
\alias{twobit_seqlengths}

\title{Extract sequence lengths and letter counts from a .2bit file}

\description{
  Extract the lengths and letter counts of the DNA sequences stored
  in a \code{.2bit} file.
}

\usage{
twobit_seqstats(filepath)

twobit_seqlengths(filepath)
}

\arguments{
  \item{filepath}{
    A single string (character vector of length 1) containing a path
    to a \code{.2bit} file.
  }
}

\details{
  \code{twobit_seqlengths(filepath)} is a shortcut for
  \code{twobit_seqstats(filepath)[ , "seqlengths"]} that is also a
  much more efficient way to get the sequence lengths as it does not
  need to load the sequence data in memory.
}

\value{
  For \code{twobit_seqstats()}: An integer matrix with one row per sequence
  in the \code{.2bit} file and 6 columns. The rownames on the matrix are the
  sequence names and the colnames are: \code{seqlengths}, \code{A}, \code{C},
  \code{G}, \code{T}, \code{N}. Columns \code{A}, \code{C}, \code{G}, \code{T},
  and \code{N} contain the letter count for each sequence.

  For \code{twobit_seqlengths()}: A named integer vector where the names
  are the sequence names and the values the corresponding lengths.
}

\references{
  A quick overview of the \emph{2bit} format:
  \url{https://genome.ucsc.edu/FAQ/FAQformat.html#format7}
}

\seealso{
  \code{\link{twobit_read}} and \code{\link{twobit_write}} to read/write a
  character vector representing DNA sequences from/to a file in \emph{2bit}
  format.
}

\examples{
filepath <- system.file(package="Rtwobitlib", "extdata", "sacCer2.2bit")

twobit_seqstats(filepath)

twobit_seqlengths(filepath)

## Sanity checks:
sacCer2_seqstats <- twobit_seqstats(filepath)
stopifnot(
  identical(sacCer2_seqstats[ , 1], twobit_seqlengths(filepath)),
  all.equal(rowSums(sacCer2_seqstats[ , -1]), sacCer2_seqstats[ , 1])
)
}

\keyword{manip}
