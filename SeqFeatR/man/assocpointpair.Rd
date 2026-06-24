\name{assocpointpair}
\alias{assocpointpair}
\title{Compare results of SeqFeatRs assocpoint with SeqFeatRs assocpair,
  assocpairfeat}
  
\description{Calculates which significant positions in result of assocpoint
  are also in results of assocpair/assocpairfeat.}
  
\usage{assocpointpair(path_to_file_sequence_alignment = NULL, 
    path_to_file_assocpoint_csv_result = NULL, 
    path_to_file_assocpairfeat_csv_result = NULL,
    significance_level = 0.05, save_name_csv, save_name_pos)}
    
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. For reference
    see example file.}
  \item{path_to_file_assocpoint_csv_result}{file with result from SeqFeatRs assocpoint.
  For reference see example file.}
  \item{path_to_file_assocpairfeat_csv_result}{file with results from SeqFeatRs assocpair,
  assocpairfeat. For reference see example file.}
  \item{significance_level}{p-value to be defined as significant.}
  \item{save_name_csv}{name of file to which results are saved in
  csv format.}
  \item{save_name_pos}{name of file to which results are saved in csv format
  (contains possible compensatory mutations).}
}

\details{
  Takes the results from assocpoint and assocpair/assocpairfeat and
  tries to combine them into one result: we are looking for pairs of
  sequence alignment positions where (a) each of the positions is significantly associated
  with the feature (for significance level see parameter
  significance_level), and (b) the sequence states of both positions are
  associated. Pairs of positions fulfilling these two criteria may e.g.
  carry compensatory mutations.
  If at least one such mutation is found, a detailed output is additionally
  created with possible compensatory mutations}

\value{A csv file with p-value for alignment position pairs and if existing
       a csv file with possible compensatory mutations.}

\author{Bettina Budeus}

\seealso{\code{\link{assocpoint}}

\code{\link{assocpairfeat}}

\code{\link{assocpair}}}

\examples{
#Input files
\dontrun{
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
assocpoint_result <- system.file("extdata", "assocpoint_results.csv", package="SeqFeatR")
assocpairfeat_result <- system.file("extdata", "assocpairfeat_results.csv", package="SeqFeatR")

#Usage
assocpointpair(
	path_to_file_sequence_alignment=fasta_input, 
	path_to_file_assocpoint_csv_result=assocpoint_result, 
	path_to_file_assocpairfeat_csv_result=assocpairfeat_result, 
	significance_level=0.05,
	save_name_csv="assocpointpair_result.csv",
	save_name_pos="possible_compensatory_mutation.csv")
}}
