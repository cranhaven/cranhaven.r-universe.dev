\name{assoctuple}
\alias{assoctuple}
\title{Searches for associations of a tuple of alignment positions with feature(s)}

\description{Searches for associations of tuple of nucleotide or amino
  acid sequence alignment positions with feature(s).}
  
\usage{assoctuple(path_to_file_sequence_alignment, path_to_file_assocpoint_csv_result, threshold,
	min_number_of_elements_in_tuple, max_number_of_elements_in_tuple,
	save_name_csv, column_of_feature, column_of_position, column_of_p_values, 
        column_of_aa, A11, A12, A21, A22, B11, B12, B21, B22, one_feature, feature)}
	
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. For reference
    see example file.}
  \item{path_to_file_assocpoint_csv_result}{results from SeqFeatRs assocpoint}
  \item{threshold}{p-value threshold for sequence alignment positions to
    be considered.}
  \item{min_number_of_elements_in_tuple}{minimal number of members in tuple.}
  \item{max_number_of_elements_in_tuple}{maximal number of members in tuple.}
  \item{save_name_csv}{name of file to which results are saved in csv format.}
  \item{column_of_feature}{column number in which feature is located for which analysis should be done.}
  \item{column_of_position}{column number in which sequence position is located.}
  \item{column_of_p_values}{column number from which p-values should be taken. See details.}
  \item{column_of_aa}{column number from which amino acids should be taken. See details.}
  \item{A11}{position of start of first HLA A allele in header line of FASTA file.}
  \item{A12}{position of end of first HLA A allele in header line of FASTA file.}
  \item{A21}{position of start of second HLA A allele in header line of FASTA file.}
  \item{A22}{position of end of second HLA A allele in header line of FASTA file.}
  \item{B11}{position of start of first HLA B allele in header line of FASTA file.}
  \item{B12}{position of end of first HLA B allele in header line of FASTA file.}
  \item{B21}{position of start of second HLA B allele in header line of FASTA file.}
  \item{B22}{position of end of second HLA B allele in header line of FASTA file.}
  \item{one_feature}{if there is only one feature.}
  \item{feature}{feature identifier which should be analyzed. See details.}
}
\details{
  For each tuple of sequence alignment positions, Fisher's exact test is
  evaluated for a 2-by-2 contingency table of amino acid tuple
  (or nucleic acid tuple) vs. feature. The resulting p-values are returned in a table.

  For this to work properly the result from SeqFeatRs assocpoint can be used, 
  but also a user generated csv file in which at least one column describes the
  sequence position and another the p-value at this position, as well as a
  FASTA file from which those file originated.

  assoctuple takes only those sequence positions which have a p-value lower than
  the given threshold ('threshold'). Be aware that for big datasets the 
  calculation time can be high and a calculation of every position with
  every other position will most definitely result in low quality data.

  Please be also aware that it just uses the position in your csv file.
  If this is NOT the correct position, because of removal of empty or near empty
  alignment positions, correct the csv file before starting. 

  Use the same FASTA file you had used for assocpoint!

  The sequence positions to be included in this analysis are normally chosen
  from the corrected p-values (but can be anything else as long as it
  is between 0 and 1, even an own added column). 
  The size of the tuples can be anything from 2 to number of rows (= number
  of alignment positions) in the csv input file.
  The input sequence alignment may be consist either of DNA sequences
  (switch dna = TRUE) or amino acid sequences (dna =
  FALSE). Undetermined nucleotides or amino acids have to be indicated
  by the letter "X". 

  Features may be HLA types, indicated by four blocks in the FASTA
  comment lines. The positions of these blocks in the comment lines are
  defined by parameters A11, ..., B22. For patients with a homozygous
  HLA allele the second allele has to be "00" (without the double
  quotes). For non-HLA-type features, set option one_feature=TRUE. The
  value of the feature (e.g. 'yes / no', or '1 / 2 / 3') should then be
  given at the end of each FASTA comment, separated from the part before
  that by a semicolon. 

  The analysis is done only for one single feature. This is chosen by either
  'feature' if there is only 'one_feature', or column_of_feature if
  there are HLA types.

}

\value{A csv list of tuple positions and the p-value of there association.}

\author{Bettina Budeus}

\seealso{\code{\link{assocpoint}}}

\examples{
#Input files
\dontrun{
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
assocpoint_result <- system.file("extdata", "assocpoint_results.csv", package="SeqFeatR")

#Usage
assoctuple(
	path_to_file_sequence_alignment=fasta_input,
	path_to_file_assocpoint_csv_result=assocpoint_result,
	threshold=0.2,
	min_number_of_elements_in_tuple=2,
	max_number_of_elements_in_tuple=2,
	save_name_csv="assoctuple_result.csv",
	column_of_feature=9,
	column_of_position=1,
	column_of_p_values=9,
	column_of_aa=12,
	A11=10,
	A12=11,
	A21=13,
	A22=14,
	B11=17,
	B12=18,
	B21=20,
	B22=21,
	one_feature=FALSE,
	feature="")
}
}
