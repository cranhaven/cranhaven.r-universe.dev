\name{assocpoint}
\alias{assocpoint}
\title{Searches for associations of single alignment positions with feature(s)}

\description{Searches for associations of single nucleotide or amino
  acid sequence alignment positions with feature(s).}
  
\usage{assocpoint(path_to_file_sequence_alignment = NULL,
    path_to_file_known_epitopes = NULL, path_to_file_binding_motifs = NULL, save_name_pdf,
    save_name_csv, dna = FALSE, 
    patnum_threshold = 1, optical_significance_level = 0.05, 
    star_significance_level = 0.001, A11, A12, A21, A22, B11, B12, B21, 
    B22, C11, C12, C21, C22, has_C=FALSE, multiple_testing_correction = "bonferroni",
    bayes_factor = FALSE, constant_dirichlet_precision_parameter=FALSE,
    dirichlet_precision_parameter=20, phylo_bias_check = FALSE,
    path_to_file_reference_sequence=NULL, one_feature=FALSE,
    window_size=9, epi_plot=FALSE)}
    
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. For reference
    see example file.}
  \item{path_to_file_known_epitopes}{csv file with known epitopes. See example file.}
  \item{path_to_file_binding_motifs}{csv file with HLA binding motifs if
    available. See example file.}
  \item{save_name_pdf}{name of file to which results are saved in pdf format.}
  \item{save_name_csv}{name of file to which results are saved in csv format.}
  \item{dna}{DNA or amino acid sequences.}
  \item{patnum_threshold}{minimum number of patients per HLA type to consider in calculation.}
  \item{optical_significance_level}{height of horizontal line in
    graphical output indicating significance level, e.g.\ 0.05.}
  \item{star_significance_level}{height of invisible horizontal line
    above which all points are marked as stars, e.g.\ 0.001 as level for high
    significance.}
  \item{A11}{position of start of first HLA A allele in header line of FASTA file.}
  \item{A12}{position of end of first HLA A allele in header line of FASTA file.}
  \item{A21}{position of start of second HLA A allele in header line of FASTA file.}
  \item{A22}{position of end of second HLA A allele in header line of FASTA file.}
  \item{B11}{position of start of first HLA B allele in header line of FASTA file.}
  \item{B12}{position of end of first HLA B allele in header line of FASTA file.}
  \item{B21}{position of start of second HLA B allele in header line of FASTA file.}
  \item{B22}{position of end of second HLA B allele in header line of FASTA file.}
  \item{C11}{position of start of first HLA C allele in header line of FASTA file.}
  \item{C12}{position of end of first HLA C allele in header line of FASTA file.}
  \item{C21}{position of start of second HLA C allele in header line of FASTA file.}
  \item{C22}{position of end of second HLA C allele in header line of FASTA file.}
  \item{has_C}{set to TRUE if there is a C allel information in header line of FASTA file.}
  \item{multiple_testing_correction}{multiple testing correction applied to p-values. Input can be: "holm",\cr
 "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".}
  \item{bayes_factor}{if bayes factor should be applied instead of Fisher's exact test for association. See details.}
  \item{constant_dirichlet_precision_parameter}{if Dirichlet precision parameter K is a fixed value. See details.}
  \item{dirichlet_precision_parameter}{the Dirichlet precision parameter for evaluation with Bayes factor. See details.}
  \item{phylo_bias_check}{simple mechanism to detect phylogenetic bias in data. See details.}
  \item{path_to_file_reference_sequence}{Reference sequence for orientation. Will be
    added as extra column in the results. Not used in calculation itself.}
  \item{one_feature}{if there is only one feature. See details.}
  \item{window_size}{size of sliding window in graphical output.}
  \item{epi_plot}{add epitope plot to results pdf file.}
}
\details{
  For each sequence alignment position (each column of sequence
  alignment), Fisher's exact test is evaluated for a 2-by-2 contingency
  table of amino acid (or nucleic acid) vs. feature. The resulting
  p-values are returned in a table. p-values can be corrected for
  multiple testing using various methods (option
  multiple_testing_correction).

  If bayes_factor is TRUE, a simple calculation of a bayes_factor is applied instead of the
  Fisher's exact test. An independent model is compared with a nearly
  independent model (see. Mixtures of Dirichlet Distributions and Estimation in Contingency Tables).
  The user can choose if a fixed dirichlet precision parameter or a calculated one should be used.
  The calculated dirichlet precision parameter is dependend from the number of entries in
  the 2-by-2 contingency table.
  For more information see the mentioned paper and "Bayesian Computation with R". With Bayes Factor
  a usage of multiple testing is not possible (and in most of all cases not needed). 
  If constant_dirichlet_precision_parameter is FALSE, the Dirichlet precision parameter K
  for the calculation of the Bayes Factors is calculated by the system.
  
  The input sequence alignment may be consist either of DNA sequences
  (switch dna = TRUE) or amino acid sequences (dna =
  FALSE). Undetermined nucleotides or amino acids have to be indicated
  by the letter "X".

  A phylogenetic bias check can be made if bayes_factor = FALSE and the result added as an extra column
  to the csv result file. For each alignment position with a p-value below a
  user given threshold (optical_significance_level), the mean distance of all
  sequences with the mutation, which gave rise to this p-value, is calculated
  and compared with the mean distance of all sequences.

  Features may be either a single feature, or HLA types, the latter indicated by four blocks in the FASTA
  comment lines. The positions of these blocks in the comment lines are
  defined by parameters A11, ..., B22 (..C22 if there is information about the C allel).
  For patients with a homozygous HLA allele the second allele has to be "00" (without the double
  quotes). For single features (no HLA types), set option one_feature=TRUE. The
  value of the feature (e.g. 'yes / no', or '1 / 2 / 3') should then be
  given at the end of each FASTA comment, separated from the part before
  that by a semicolon.
}

\value{The function generates various types of output: a table of
  p-values, p-values corrected for multiple testing, z-scores, amino acid with
  the lowest p-value at this position, result of the phylogenetic bias test,
  and several plots. A Manhattan plot is generated
  for each feature in a pdf file with alignment positions on the x-axis and
  p-values on the y-axis.
  Two different significance levels can be indicated by a line
  (optical_significance_level) and the change from the normal dot symbol to
  a star (star_significance_level).

  In case of bayes_factor = TRUE, the Bayes Factor and not a p-value and
  the estimate of the simulation standard error of the computed value of the
  Bayes factor is given.

  Optionally (epi_plot = TRUE), a second plot is provided with the number
  of 'significant' p-values or Bayes Factors (value of significance chosen by
  optical_significance_level) in a sliding window of x amino acids
  ( x can be any number from 1 up to the length of the alignment chosen
  by 'window_size'. The typical length of an MHC I binding motif is nine).
  Each point in this plot is the number of 'significant' p-values or Bayes Factors
  in the next x alignment positions. Additionally, with given HLA motifs
  (path_to_file_binding_motifs), a second line is added.
  This line shows potential epitopes. A potential epitope is assigned if 
  a 'significant' p-value or Bayes Factor
  (value of significance chosen by optical_significance_level) is inside one
  of the motifs for this HLA type.
}


\author{Bettina Budeus}

\references{
Albert, James H.; Gupta, Arjun K.
Mixtures of Dirichlet Distributions and Estimation in Contingency Tables. 
Ann. Statist. 10 (1982), no. 4, 1261--1268. 
doi:10.1214/aos/1176345991. 
http://projecteuclid.org/euclid.aos/1176345991.

Albert, J.
Bayesian Computation with R.
Springer; 2nd ed. 2009 edition (May 15, 2009)
}


\seealso{\code{\link{assocpointhierarchical}}, \code{\link{assocpointpair}}, \code{\link{orPlot}}}

\examples{
#Input files
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
epitopes_input <- system.file("extdata", "Example_epitopes_aa.csv", package="SeqFeatR")
motifs_input <- system.file("extdata", "Example_HLA_binding_motifs_aa.csv", package="SeqFeatR")
reference_input <- system.file("extdata", "Example_reference_aa.fasta", package="SeqFeatR")

#Usage
\dontrun{
assocpoint(
	path_to_file_sequence_alignment=fasta_input,
	path_to_file_known_epitopes=epitopes_input,
	path_to_file_binding_motifs=motifs_input,
	save_name_pdf="assocpoint_results.pdf",
	save_name_csv="assocpoint_results.csv",
	dna=FALSE,
	patnum_threshold=1,
	optical_significance_level=0.01, 
	star_significance_level=0.5,
	A11=10,
	A12=11,
	A21=13,
	A22=14,
	B11=17,
	B12=18,
	B21=20,
	B22=21,
	C11=24,
	C12=25,
	C21=27,
	C22=28,
	has_C=FALSE,
	multiple_testing_correction="bonferroni",
	bayes_factor=FALSE,
	constant_dirichlet_precision_parameter=TRUE,
	dirichlet_precision_parameter=20,
	phylo_bias_check=FALSE,
	path_to_file_reference_sequence=reference_input,
	one_feature=FALSE,
	window_size=9,
	epi_plot=TRUE)
}
}
