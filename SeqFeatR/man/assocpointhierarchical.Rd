\name{assocpointhierarchical}
\alias{assocpointhierarchical}
\title{Searches for associations of single alignment positions with feature(s) with a simple hierarchical model}

\description{Searches for associations of single nucleotide or amino
  acid sequence alignment positions with feature(s).}
  
\usage{assocpointhierarchical(path_to_file_sequence_alignment = NULL,
    path_to_file_known_epitopes = NULL, path_to_file_binding_motifs = NULL, save_name_pdf,
    save_name_csv, dna = FALSE, 
    patnum_threshold = 1, optical_significance_level = 0.05, 
    star_significance_level = 0.001, A11, A12, A21, A22, B11, B12, B21, 
    B22, path_to_file_reference_sequence=NULL, one_feature=FALSE,
    window_size=9, epi_plot=FALSE, own_model, number_of_simulations,
    number_of_burnin, number_of_chains, response_inits,	further_response_data,
    response_parameters)}
    
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
  \item{path_to_file_reference_sequence}{Reference sequence for orientation. Will be
    added as extra column in the results. Not used in calculation itself.}
  \item{one_feature}{if there is only one feature. See details.}
  \item{window_size}{size of sliding window in graphical output.}
  \item{epi_plot}{add epitope plot to results pdf file.}
  \item{own_model}{txt file with jags model - optional. See example file.}
  \item{number_of_simulations}{number of total iterations per chain (including burn in) - see R2jags package.}
  \item{number_of_burnin}{length of burn in, i.e. number of iterations to discard at the beginning - see R2jags package.}
  \item{number_of_chains}{number of Markov chains - see R2jags package.}
  \item{response_inits}{specification of initial value - see details and R2jags package.}
  \item{further_response_data}{additional response data - see details and rjags/R2jags package.}
  \item{response_parameters}{name of response parameters - see details and rjags/R2jags package.}

}
\details{
  For a given alignment a simple hierarchical model (amino acids/ nucleotides at each position
  are multi-nominally distributed, features are gamma(0.1,0.1) distributed - see example file).

  The input sequence alignment may be consist either of DNA sequences
  (switch dna = TRUE) or amino acid sequences (dna =
  FALSE). Undetermined nucleotides or amino acids have to be indicated
  by the letter "X".

  Features may be either a single feature, or HLA types, the latter indicated by four blocks in the FASTA
  comment lines. The positions of these blocks in the comment lines are
  defined by parameters A11, ..., B22. For patients with a homozygous
  HLA allele the second allele has to be "00" (without the double
  quotes). For single features (no HLA types), set option one_feature=TRUE. The
  value of the feature (e.g. 'yes / no', or '1 / 2 / 3') should then be
  given at the end of each FASTA comment, separated from the part before
  that by a semicolon.

  However a user can add his own model and parameter (only for EXPERTS!):
  The jags model file has to be given as 'own_model'. This 'activates' the following parameters:
  1. Further, the user can add other response data ('further_response_data') besides the internally already given
  matrix with all amino acids/nucleotides at every position and every feature and the colSums of this matrix.
  2. The user has to insert initial values ('response_inits') and 
  3. response parameters ('response_parameters').
  For the definition of initial values the following variables can be used:
  - all_of_letters = all amino acids/nucleotides plus gap symbol and unknown (X)
  - min_FASTA_length = length of alignment
  - allels = all HLA-allels/features
  A response parameter named theta will be evaluated by the part of the program after the model,
  therefore the user should keep in mind to name this in his own model:
  The result theta has to be of the following form: 3 dimensional, with first dimension: feature vs no feature,
  second dimension: amino acid/nucleotide, third dimension: position (see examples file).
}

\value{The function generates various types of output: a table of
  probabilities, z-scores, amino acid/nucleotide with the highest probability at this position and several plots.
  A Manhattan plot is generated for each feature in a pdf file with alignment positions
  on the x-axis and probabilities on the y-axis.
  Two different significance levels can be indicated by a line
  (optical_significance_level) and the change from the normal dot symbol to
  a star (star_significance_level).

  Optionally (epi_plot = TRUE), a second plot is provided with the number
  of 'significant' p-values (value of significance chosen by optical_significance_level)
  in a sliding window of x amino acids ( x can be any number from 1 up to the
  length of the alignment chosen by 'window_size'. The typical length of an MHC I binding motif is nine).
  Each point in this plot is the number of 'significant' probabilities in the next x
  alignment positions. Additionally, with given HLA motifs
  (path_to_file_binding_motifs), a second line is added.
  This line shows potential epitopes. A potential epitope is assigned if 
  a 'significant' probabilities (value of significance chosen by optical_significance_level)
  is inside one of the motifs for this HLA type. 
}

\note{
  To uses this function, it is essential to install Jags. This seems to be easy for Windows
  and Linux systems, but complicated for MacOS. Therefore this function is not tested in
  installation process.
}


\author{Bettina Budeus}

\references{
Albert, J.
Bayesian Computation with R.
Springer; 2nd ed. 2009 edition (May 15, 2009)
}


\seealso{\code{\link{assocpoint}}, \code{\link{assocpointpair}}}

\examples{
#Input files
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
epitopes_input <- system.file("extdata", "Example_epitopes_aa.csv", package="SeqFeatR")
motifs_input <- system.file("extdata", "Example_HLA_binding_motifs_aa.csv", package="SeqFeatR")
own_model_input <- system.file("extdata", "Example_model_for_bayes.txt", package="SeqFeatR")
reference_input <- system.file("extdata", "Example_reference_aa.fasta", package="SeqFeatR")

#Usage
\dontrun{
assocpointhierarchical(
	path_to_file_sequence_alignment=fasta_input,
	path_to_file_known_epitopes=epitopes_input,
	path_to_file_binding_motifs=motifs_input,
	save_name_pdf = "assocpointhierarchical_results.pdf",
	save_name_csv = "assocpointhierarchical_results.csv",
	dna = FALSE,
	patnum_threshold = 1,
	optical_significance_level = 97, 
	star_significance_level = 99,
	A11 = 10,
	A12 = 11,
	A21 = 13,
	A22 = 14,
	B11 = 17,
	B12 = 18,
	B21 = 20,
	B22 = 21,
	path_to_file_reference_sequence = reference_input,
	one_feature=FALSE,
	window_size=9,
	epi_plot=TRUE,
	own_model=own_model_input,
	number_of_simulations=20,
	number_of_burnin=floor(20/2),
	number_of_chains=2,
	response_inits="'vcounts' = rep(1, length(all_of_letters)), 
       'theta' = array(1/length(all_of_letters),
        dim=c(2, length(all_of_letters), min_FASTA_length))",
	further_response_data=c(),
	response_parameters=c("theta", "vcounts"))
}
}
