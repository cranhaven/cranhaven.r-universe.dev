\name{Annotation}
\alias{Annotation}
\alias{track.table.GTF}
\alias{track.exons.CCDS}
\alias{track.exons.GENCODE}
\alias{track.CNV.DGV}
\alias{track.genes.NCBI}
\alias{track.bands.UCSC}

\title{
  Annotation track constructors
}

\description{
  These functions constructs \code{\linkS4class{track.table}} inheriting objects from free annotation files.
}

\usage{
  track.table.GTF(file, name = NA, attr = "split", features = "exon", quiet = FALSE, 
    .chromosomes, ...)
  track.exons.CCDS(file, name = "CCDS exons", ...)
  track.exons.GENCODE(file, name = "GENCODE exons",
    extra = c("gene_id", "gene_name", "exon_id"), ...)
  track.CNV.DGV(file, name = "DGV CNV", ...)
  track.genes.NCBI(file, name = "NCBI genes", selection, ...)
  track.bands.UCSC(file, name = "UCSC bands", ...)
}

\arguments{
  \item{file}{
    Single character value, the path to the raw file to parse. See the 'Details' section below.
}
  \item{name}{
    Single character value, the \code{name} field for the \code{\linkS4class{track.table}} object. For \code{track.table.GTF}, use \code{NA} to refer to the "source" column content (if it contains a unique value for all rows).
}
  \item{attr}{
    To be passed to \code{\link{read.gtf}}.
}
  \item{features}{
    To be passed to \code{\link{read.gtf}}.
}
  \item{quiet}{
    To be passed to \code{\link{read.gtf}}.
}
  \item{.chromosomes}{
    To be passed to the \code{\link{track.table}} constructor.
}
  \item{\dots}{
    Further arguments are passed to the class constructor, as a result most of the handled arguments are \code{\link{track.table}} arguments. Consider notably \code{.organism} and \code{.assembly} for track annotation.
}
  \item{extra}{
    Character vector, names of optional columns to keep from the GENCODE GTF file.
}
  \item{selection}{
    Character vector, filter to apply on the "group_label" column for NCBI genes. Raises an error with the possible values when missing.
}
}

\details{
  \code{track.table.GTF} imports a "Gene Feature Transfert" file, as proposed by the UCSC Table Browser at \url{http://www.genome.ucsc.edu/cgi-bin/hgTables}) for a large amount of species. See the \code{\link{read.gtf}} manual for further details.
  
  \code{track.exons.CCDS} contains various transcripts from the "Consensus Coding DNA Sequence" project, currently only available for mouse and human (see the NCBI data repository at \url{https://ftp.ncbi.nlm.nih.gov/pub/CCDS/}, and look for a file named "CCDS_current.txt").
  
  \code{track.exons.GENCODE} contains various transcripts from the GENCODE project, currently only available for mouse and human (see the dedicated website at \url{https://www.gencodegenes.org/}). This function is intended to run on the GTF version of the data.
  
  \code{track.CNV.DGV} parses constitutive copy number variations from the current version of the Database of Genomic Variants, downloadable from \url{http://dgv.tcag.ca/dgv/app/downloads} using "DGV Variants" links. The whole database is dedicated to the human specy only.
  
  \code{track.genes.NCBI} parses the gene list from the MapView project of the NCBI, for one of many species available at \url{https://ftp.ncbi.nih.gov/genomes/MapView/}. Select your specy of interest, then browse "sequence", "current" and "initial_release" (if the directories are available, they are not for certain species). Download the file named "seq_gene.md.gz". As many assemblies are included in the file, a first call to the function without "selection" is required, to list the available values. A second call with the appropriate assembly name will produce the desired track file.

  \code{track.bands.UCSC} produces a track of cytogenetic banding, as made available by the UCSC for many species at \url{http://hgdownload.cse.ucsc.edu/downloads.html}. Select the specy and assembly version that suits your needs, and look for a file named "cytoBand.txt.gz" in the "Annotation database" section.
}

\value{
  Return a \code{\linkS4class{track.table}}-inheriting object (of class \code{\linkS4class{track.exons}}, \code{\linkS4class{track.CNV}}, \code{\linkS4class{track.genes}} or \code{\linkS4class{track.bands}}).
}

\references{
  Example of \code{track.exons.CCDS} raw file (current human assembly) : \url{https://ftp.ncbi.nlm.nih.gov/pub/CCDS/current_human/CCDS.current.txt}
  
  Example of \code{track.exons.GENCODE} raw file (human assembly 'GRCh38') : \url{ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gtf.gz}
  
  Example of \code{track.CNV.DGV} raw file (human assembly 'hg19') : \url{http://dgv.tcag.ca/dgv/docs/GRCh37_hg19_variants_2013-05-31.txt}
  
  Example of \code{track.genes.NCBI} raw file (human assembly 'GRCh37') : \url{https://ftp.ncbi.nih.gov/genomes/MapView/Homo_sapiens/sequence/BUILD.37.3/initial_release/seq_gene.md.gz}

  Example of \code{track.bands.UCSC} raw file (human assembly 'hg19') : \url{http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{track.fasta-constructors}}, \code{\link{Annotation}}
  
  \code{\link{track.table-class}}, \code{\link{track.exons-class}}, \code{\link{track.CNV-class}}, \code{\link{track.genes-class}} or \code{\link{track.bands-class}}

  \code{\link{tk.browse}}, \code{\link{browsePlot}}
}

\examples{
  # From the "How-to" vignette, section "Custom annotation tracks"
  file <- system.file("extdata/Cosmic_ATM.gtf.gz", package="Rgb")
  tt <- track.table.GTF(file)
}

