\name{foundereffectfinder}
\alias{foundereffectfinder}
\title{Tries to identify a founder effect in results from SeqFeatRs assocpair}

\description{Takes the result of co-mutation analysis like SeqFeatRs assocpair function and adds a
 column with a note if the row is in the same branch.}
 
\usage{foundereffectfinder(path_to_file_sequence_alignment = NULL,
    path_to_file_assocpair_csv_result = NULL, path_to_file_nexus_tree = NULL,
    save_name_csv, threshold = 7)}
    
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. For reference
    see example file.}
  \item{path_to_file_assocpair_csv_result}{csv file with results from SeqFeatRs assocpair function.
  For reference see example file.}
  \item{path_to_file_nexus_tree}{nexus file with tree data for the given sequence
  alignment. For reference see example file.}
  \item{save_name_csv}{name of file to which results are saved in csv format.}
  \item{threshold}{threshold for the number of allowed occurrences of tips
  in one branch.}
}

\details{Uses the results from SeqFeatRs assocpair analysis and a generated tree
 for the same sequences in nexus format
 (for tree generation, e.g http://mafft.cbrc.jp/alignment/server/ can be used).
 For every pair of amino acids the number of sequences with both amino acids
 inside one branch is calculated. 
 If this number exceeds 'threshold' times the number of found associations, this
 association is considered to be due to a founder effect.}

\value{Input file with an added column with founder effect information.}

\references{Mayr, Ernst (1954). "Change of genetic environment and evolution". In Julian Huxley. 
Evolution as a Process. London: George Allen & Unwin. OCLC 974739}

\author{Bettina Budeus}

\note{Only use files generated with SeqFeatRs assocpair or assoctuple.}

\seealso{\code{\link{assocpair}}}

\examples{
#Input files
\dontrun{
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
assocpair_result <- system.file("extdata", "assocpair_results.csv", package="SeqFeatR")
tree_input <- system.file("extdata", "Example_tree.nh", package="SeqFeatR")

#Usage
foundereffectfinder(
	path_to_file_sequence_alignment=fasta_input,
	path_to_file_assocpair_csv_result=assocpair_result,
	path_to_file_nexus_tree=tree_input,
	save_name_csv="foundereffectfinder_result.csv",
	threshold=7)
}}
