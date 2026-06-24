\name{assocpairfeat}
\alias{assocpairfeat}
\title{Finding pairs of sequence alignment positions associated with sequence feature.}

\description{Determines pairs of alignment positions that jointly mutate 
  depending on sequence feature.}
  
\usage{assocpairfeat(path_to_file_sequence_alignment = NULL, save_name_csv, dna = FALSE, 
    patnum_threshold = 1, significance_level = 0.05, 
    A11a, A12a, A21a, A22a, B11a, B12a, B21a, B22a,
    multiple_testing_correction = "bonferroni")}
    
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. See example file.}
  \item{save_name_csv}{name of file to which results are written in csv format.}
  \item{dna}{DNA or amino acid sequences.}
  \item{patnum_threshold}{minimum number of patients per HLA type to consider in calculation.}
  \item{significance_level}{significance level in Fisher's exact test.}
  \item{A11a}{position of start of first HLA A allele in header line of FASTA file.}
  \item{A12a}{position of end of first HLA A allele in header line of FASTA file.}
  \item{A21a}{position of start of second HLA A allele in header line of FASTA file.}
  \item{A22a}{position of end of second HLA A allele in header line of FASTA file.}
  \item{B11a}{position of start of first HLA B allele in header line of FASTA file.}
  \item{B12a}{position of end of first HLA B allele in header line of FASTA file.}
  \item{B21a}{position of start of second HLA B allele in header line of FASTA file.}
  \item{B22a}{position of end of second HLA B allele in header line of FASTA file.}
  \item{multiple_testing_correction}{multiple testing correction applied to p-values. Input can be: "holm",\cr
 "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".}
}
\details{
  Features may be HLA types, indicated by four blocks in the FASTA
  comment lines. The positions of these blocks in the comment lines are
  defined by parameters A11, ..., B22. For patients with a homozygous
  HLA allele the second allele has to be "00" (without the double
  quotes). 

  For every position in the sequence alignment from the FASTA
  file a Fisher's exact test is applied with every other position in the
  sequence and every HLA-type. Significant p-values are collected in one
  big table. p.adjust from stats package is used for multiple testing
  correction; corrected values are given as extra column in csv output.
  
  In contrast to assocpair, assocpairfeat uses an analysis with 'features'
  without a consensus sequence.
}

\value{Table with all alignment position pairs with
  significant association with sequence feature.}

\author{Bettina Budeus}

\note{Use \code{\link{visualizepairfeat}} for graphical output.}

\seealso{\code{\link{visualizepairfeat}}, \code{\link{assocpair}}}

\examples{
#Input file
\dontrun{
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")

#Usage
assocpairfeat(
	path_to_file_sequence_alignment=fasta_input,
	save_name_csv="assocpairfeat_results.csv",
	dna=FALSE,
	patnum_threshold=1,
	significance_level=0.05,
	A11=10,
	A12=11,
	A21=13,
	A22=14,
	B11=17,
	B12=18,
	B21=20,
	B22=21,
	multiple_testing_correction="bonferroni")
}
}
