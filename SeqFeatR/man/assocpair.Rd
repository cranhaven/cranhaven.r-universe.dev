\name{assocpair}
\alias{assocpair}
\title{Finding pairs of alignment positions that jointly mutate}

\description{Determines pairs of sequence alignment positions that
  mutate in a correlated fashion with respect to a consensus sequence.}
  
\usage{assocpair(path_to_file_sequence_alignment = NULL,
    path_to_file_consensus = NULL, save_name_csv, dna = FALSE,
    significance_level = 0.05, multiple_testing_correction = "bonferroni")}
    
\arguments{
  \item{path_to_file_sequence_alignment}{FASTA file with sequence alignment. See example file.}
  \item{path_to_file_consensus}{FASTA file with consensus sequence. See example file.}
  \item{save_name_csv}{name of file to which results are written in csv format.}
  \item{dna}{indicates whether sequences are DNA or amino acids.}
  \item{significance_level}{significance level for Fisher's exact test.}
  \item{multiple_testing_correction}{multiple testing correction applied to p-values. Input can be: "holm",\cr
 "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".}
}
\details{For every position in the sequence alignment from the FASTA
  file a Fisher's exact test is applied with every other position in the
  sequence to check whether at both positions we have correlated
  mutations with respect to a given consensus sequence. Significant p-values are
  collected in one big table. p.adjust from stats package is used for multiple testing
  correction; corrected values are given as extra column in csv output.
  
  In contrast to assocpairfeat, assocpair does not use features, but uses a consensus
  approach. Please be sure, that this is really what you want to use. Otherwise, use
  assocpairfeat or assoctuple instead.
  }
  
\value{A csv file with every possible co-mutation below the given p-value.}

\author{Bettina Budeus}

\note{For graphical output use:\cr
\code{\link{visualizepair}}.}


\seealso{\code{\link{visualizepairfeat}}, \code{\link{assocpairfeat}}, \code{\link{assoctuple}}}

\examples{
#Input files
\dontrun{
fasta_input <- system.file("extdata", "Example_aa.fasta", package="SeqFeatR")
consensus_input <- system.file("extdata", "Example_Consensus_aa.fasta", package="SeqFeatR")
	
#Usage
assocpair(
	path_to_file_sequence_alignment=fasta_input,
	path_to_file_consensus=consensus_input,
	save_name_csv="assocpair_results.csv",
	dna=FALSE,
	significance_level=0.05,
	multiple_testing_correction="bonferroni")
}
}
