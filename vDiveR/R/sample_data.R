#' DiMA (v5.0.9) JSON converted-CSV Output Sample 1
#'
#' A dummy dataset with two proteins (A and B) from one host, human
#'
#'
#' @format A data frame with 806 rows and 17 variables:
#' \describe{
#'   \item{proteinName}{name of the protein}
#'   \item{position}{starting position of the aligned, overlapping k-mer window}
#'   \item{count}{number of k-mer sequences at the given position}
#'   \item{lowSupport}{k-mer position with sequences lesser than the minimum support threshold (TRUE) are considered of low support, in terms of sample size}
#'   \item{entropy}{level of variability at the k-mer position, with zero representing completely conserved}
#'   \item{indexSequence}{the predominant sequence (index motif) at the given k-mer position}
#'   \item{index.incidence}{the fraction (in percentage) of the index sequences at the k-mer position}
#'   \item{major.incidence}{the fraction (in percentage) of the major sequence (the predominant variant to the index) at the k-mer position}
#'   \item{minor.incidence}{the fraction (in percentage) of minor sequences (of frequency lesser than the major variant, but not singletons) at the k-mer position}
#'   \item{unique.incidence}{the fraction (in percentage) of unique sequences (singletons, observed only once) at the k-mer position}
#'   \item{totalVariants.incidence}{the fraction (in percentage) of sequences at the k-mer position that are variants to the index (includes: major, minor and unique variants)}
#'   \item{distinctVariant.incidence}{incidence of the distinct k-mer peptides at the k-mer position}
#'   \item{multiIndex}{presence of more than one index sequence of equal incidence}
#'   \item{host}{species name of the organism host to the virus}
#'   \item{highestEntropy.position}{k-mer position that has the highest entropy value}
#'   \item{highestEntropy}{highest entropy values observed in the studied protein}
#'   \item{averageEntropy}{average entropy values across all the k-mer positions}
#' }
"proteins_1host"

#' DiMA (v5.0.9) JSON converted-CSV Output Sample 2
#'
#' A dummy dataset with 1 protein (Core) from two hosts, human and bat
#'
#'
#' @format A data frame with 200 rows and 17 variables:
#' \describe{
#'   \item{proteinName}{name of the protein}
#'   \item{position}{starting position of the aligned, overlapping k-mer window}
#'   \item{count}{number of k-mer sequences at the given position}
#'   \item{lowSupport}{k-mer position with sequences lesser than the minimum support threshold (TRUE) are considered of low support, in terms of sample size}
#'   \item{entropy}{level of variability at the k-mer position, with zero representing completely conserved}
#'   \item{indexSequence}{the predominant sequence (index motif) at the given k-mer position}
#'   \item{index.incidence}{the fraction (in percentage) of the index sequences at the k-mer position}
#'   \item{major.incidence}{the fraction (in percentage) of the major sequence (the predominant variant to the index) at the k-mer position}
#'   \item{minor.incidence}{the fraction (in percentage) of minor sequences (of frequency lesser than the major variant, but not singletons) at the k-mer position}
#'   \item{unique.incidence}{the fraction (in percentage) of unique sequences (singletons, observed only once) at the k-mer position}
#'   \item{totalVariants.incidence}{the fraction (in percentage) of sequences at the k-mer position that are variants to the index (includes: major, minor and unique variants)}
#'   \item{distinctVariant.incidence}{incidence of the distinct k-mer peptides at the k-mer position}
#'   \item{multiIndex}{presence of more than one index sequence of equal incidence}
#'   \item{host}{species name of the organism host to the virus}
#'   \item{highestEntropy.position}{k-mer position that has the highest entropy value}
#'   \item{highestEntropy}{highest entropy values observed in the studied protein}
#'   \item{averageEntropy}{average entropy values across all the k-mer positions}
#' }
"protein_2hosts"

#' DiMA (v5.0.9) JSON Output File
#'
#' A sample DiMA JSON Output File which acts as the input for JSON2CSV()
#'
#'
#' @format A Diversity Motif Analyzer (DiMA) tool JSON file
"JSON_sample"

#' Metadata Input Sample
#'
#' A dummy dataset that acts as an input for plot_world_map() and plot_time()
#'
#'
#' @format A data frame with 1000 rows and 3 variables:
#' \describe{
#'   \item{ID}{unique identifier of the sequence}
#'   \item{region}{geographical region of the sequence collection}
#'   \item{date}{collection date of the sequence}
#' }
"metadata"