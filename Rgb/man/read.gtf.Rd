\name{read.gtf}
\alias{read.gtf}
\title{
  Parses a GTF2 file
}

\description{
  This function parses a simple "Gene Transfer Format" (GTF2.2) into a data.frame, as distributed by the UCSC Table Browser.
  
  As this format is an extension of the "Gene Feature Format" (GFF3), some retro-compatibility can be expected but not guaranteed.
}

\usage{
  read.gtf(file, attr = c("split", "intact", "skip"), features = NULL, quiet = FALSE)
}

\arguments{
  \item{file}{
    Single character value, the path and name of the GTF2 file to parse (possibly gzipped).
}
  \item{attr}{
    Single character value, defining how to deal with attributes. "skip" discards the attributes data, "intact" does not process it and "split" adds a column for each attribute (identified by their names).
}
  \item{features}{
    Character vector, if not \code{NULL} only rows with a "feature" column value from this list will be kept.
}
  \item{quiet}{
    Single logical value, whether to send diagnostic messages or not.
}
}

\value{
  A \code{data.frame} with the standard GTF2 columns. The "strand" column is converted to \code{factor}, "?" are turned to \code{NA} and "." are kept for features where stranding is not relevant (See the GFF3 specification).
}

\references{
  GTF2.2 specification : \url{http://mblab.wustl.edu/GTF22.html}
  
  GFF3 Sequence Ontology specification : \url{http://www.sequenceontology.org/gff3.shtml}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{track.table.GTF}}
}

\note{
  Currently not implemented :
  \itemize{
    \item{FASTA section and sequences (error raising)}
    \item{Special character escaping (error raising)}
	\item{Attribute quotes (kept)}
	\item{Sections (all data pooled)}
    \item{Meta data (ignored)}
  }
}
