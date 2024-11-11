\name{track-constructors}
\alias{track-constructors}
\alias{track.table}
\alias{track.bam}
\alias{track.genes}
\alias{track.bands}
\alias{track.exons}
\alias{track.CNV}
\title{
  Track constructors
}

\description{
  Produces \code{\linkS4class{track.table}}-inheriting objects.
}

\usage{
  track.table(..., .name, .parameters, .organism, .assembly, .chromosomes,
    .makeNames = FALSE, .orderCols = TRUE, warn = TRUE)
  track.bam(bamPath, baiPath, addChr, quiet = FALSE, .name, .organism,
    .assembly, .parameters, warn = TRUE)
  track.genes(...)
  track.bands(...)
  track.exons(...)
  track.CNV(...)
}

\arguments{
  \item{\dots}{
    Arguments to be passed to the inherited constructor (\code{\link{refTable}} for \code{\link{track.table}}, \code{\link{track.table}} for the others). \code{track.table} has few more restrictions, see \code{\link{track.table-class}} for further details.
  }
  \item{.name}{
    Single character value, to fill the \code{name} field inherited from \code{\linkS4class{drawable}}.
  }
  \item{warn}{
    Single logical value, to be passed to the appropriate \code{check} method. Other conversion warnings rely on this value too in \code{track.table}.
  }
  \item{.parameters}{
    A \code{list} of drawing parameters, to fill the \code{parameters} field of the object.
  }
  \item{.organism}{
    Single character value, to fill the \code{organism} field of the object.
  }
  \item{.assembly}{
    Single character value, to fill the \code{assembly} field of the object.
  }
  \item{.chromosomes}{
    Single character value, levels to use for the 'chrom' column if conversion to factor is needed.
  }
  \item{.makeNames}{
    Single logical value, whether to compute the 'name' column with unique values or not. If \code{TRUE}, any existing 'name' column will be replaced.
  }
  \item{.orderCols}{
    Single logical value, whether to reorder the columns for more consistency between tracks or not.
  }
  \item{bamPath}{
    Single character value, the file name and path to a BAM file (.bam) to build a track around.
  }
  \item{baiPath}{
    Single character value, the file name and path to the corresponding BAI file (.bai) to build a track around. If missing, a guess will be tried (adding '.bai' to \code{bamPath} or remplacing '.bam' by '.bai').
  }
  \item{addChr}{
    Single logical value, whether to automatically add 'chr' ahead chromosome names when querying or not. If missing, a guess will be tried looking for chromosome names beginning by 'chr' in the BAM header declaration.
  }
  \item{quiet}{
    Single logical value, whether to throw diagnostic messages during BAI parsing or not.
  }
}

\value{
  \code{\linkS4class{track.table}} and \code{\linkS4class{track.bam}} inheriting objects.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{track.table-class}}, \code{\link{track.bam-class}}
}

\examples{
  # track.table from a data.frame
  df <- data.frame(
    chrom=1, strand="+", start=1:5, end=2:6, name=letters[1:5],
    stringsAsFactors=FALSE
  )
  track.table(df)
  
  # track.table from vectors
  track.table(chrom=1, strand="+", start=1:5, end=2:6, name=letters[1:5])
  
  # track.bam
  track.bam(system.file("extdata/ATM.bam", package="Rgb"))
}

