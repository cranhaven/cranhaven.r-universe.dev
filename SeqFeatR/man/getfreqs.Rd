\name{getfreqs}
\alias{getfreqs}
\title{Get frequencies for amino acids/nucleotides in alignment window (epitope)}
\description{Gets frequencies of amino acids/nucleotides inside an epitope out of an alignment. Epitope start and end positions must be in the range of the length of the alignment, starting with the first position of the alignment as one. You can add the whole sequence consensus, but also a part of it.}
\usage{getfreqs(path_to_file_sequence_alignment = NULL, save_csv, save_png, 
 epitope_position_start, epitope_position_end,
 path_to_file_consensus, patnum_threshold = 1, A11, A12, A21, A22,
 B11, B12, B21, B22, one_feature=FALSE)}
\arguments{
  \item{path_to_file_sequence_alignment}{a FASTA file with sequence data. For reference please look in example file.}
  \item{save_csv}{name for the csv output.}
  \item{save_png}{name for the png/svg output.}
  \item{epitope_position_start}{starting position of the epitope in the alignment.}
  \item{epitope_position_end}{ending position of the epitope in the alignment.}
  \item{path_to_file_consensus}{a FASTA file with the consensus sequence from epi_pos_start till epi_pos_end.}
  \item{patnum_threshold}{the minimum number of patients of one HLA type to consider in the calculation.}
  \item{A11}{the position of the start of the first HLA A allele in the description block of the FASTA file.}
  \item{A12}{the position of the end of the first HLA A allele in the description block of the FASTA file.}
  \item{A21}{the position of the start of the second HLA A allele in the description block of the FASTA file.}
  \item{A22}{the position of the end of the second HLA A allele in the description block of the FASTA file.}
  \item{B11}{the position of the start of the first HLA B allele in the description block of the FASTA file.}
  \item{B12}{the position of the end of the first HLA B allele in the description block of the FASTA file.}
  \item{B21}{the position of the start of the second HLA B allele in the description block of the FASTA file.}
  \item{B22}{the position of the end of the second HLA B allele in the description block of the FASTA file.}
  \item{one_feature}{if there is only one feature.}
}
\details{

  This function counts the number of amino acids NOT being the same as the consensus
  and generates an output graphic as well as a result csv file.

  The features may be HLA types, indicated by four blocks in the FASTA
  comment lines. The positions of these blocks in the comment lines are
  defined by parameters A11, ..., B22. For patients with a homozygous
  HLA allele the second allele has to be "00" (without the double
  quotes). For non-HLA-type features, set option one_feature=TRUE. The
  value of the feature (e.g. 'yes / no', or '1 / 2 / 3') should then be
  given at the end of each FASTA comment, separated from the part before
  that by a semicolon.
}
\value{A csv file with a row for each feature and two columns (absolute and relative) for the frequencies of the amino acids in the epitope.}

\author{Bettina Budeus}

\seealso{\code{\link{assocpoint}}}

\examples{
\dontrun{
#Input files
	fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
	consensus_input <- system.file("extdata", "Example_Consensus_aa.fasta", package="SeqFeatR")
	
#Usage
getfreqs(
	path_to_file_sequence_alignment=fasta_input,
	save_csv="getfreqs_result.csv",
	save_png="getfreqs_result.png",
	epitope_position_start=1,
	epitope_position_end=40,
	path_to_file_consensus=consensus_input,
	patnum_threshold=1,
	A11=10,
	A12=11,
	A21=13,
	A22=14,
	B11=17,
	B12=18,
	B21=20,
	B22=21,
	one_feature=FALSE)
}}
