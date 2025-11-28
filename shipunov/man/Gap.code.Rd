\name{Gap.code}
\alias{Gap.code}
\title{Gap coding}
\description{Gap coding of DNA nucleotide alignments}

\usage{Gap.code(seqs)}

\arguments{
\item{seqs}{Character vector of aligned (and preferably flank trimmed) DNA sequences.}
}

\details{
FastGap-like gap code nucleotide alignments ('ATGCN-' are allowed).

Encodes gap presence as 'A' and absence as 'C'.

Likely too straightforward, and only weakly optimized (really slow).
}

\value{Outputs character matrix where each column is a gapcoded position.}

\references{
Borchsenius F. 2009. FastGap 1.2. Department of Biosciences, Aarhus
University, Denmark. See "http://www.aubot.dk/FastGap_home.htm".
}

\author{Alexey Shipunov}

% \seealso{}

\examples{
write(file=file.path(tempdir(), "tmp.fasta"),  c(
 ">1\nGAAC------ATGC",
 ">2\nGAAC------TTGC",
 ">3\nGAAC---CCTTTGC",
 ">4\nGAA---------GC"))
write(file=file.path(tempdir(), "tmp_expected.fasta"), c(
 ">1\nGAAC------ATGCCA-",
 ">2\nGAAC------TTGCCA-",
 ">3\nGAAC---CCTTTGCCCA",
 ">4\nGAA---------GCA--"))
tmp <- Read.fasta(file=file.path(tempdir(), "tmp.fasta"))
expected <- Read.fasta(file=file.path(tempdir(), "tmp_expected.fasta"))
seqs <- tmp$sequence
gc <- Gap.code(seqs)
tmp$sequence <- apply(cbind(seqs, gc), 1, paste, collapse="")
identical(tmp, expected) # TRUE, isn't it?
}

\keyword{manip}
