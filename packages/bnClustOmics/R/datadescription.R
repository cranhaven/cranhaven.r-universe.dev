#' mappings
#'
#' An example of mappings needed for constructing bnInfo objects; a list of data frames, one for each omics type.
#'
#' @format a list of data frames, whose names correspond to omics types. The row names of each data frame correspond to IDs used in the data. At least one column "gene" is needed to specify gene symbol corresponding to the ID.
#'
"mappings"

#' stringint
#'
#' An example of interactions list used for constructing graphical prior (penalization and blacklist) matrices.
#'
#' @format a data frames that includes three columns, gene1, gene2 and score. Column score is optional and may be skipped when constructing prior.
#'
"stringint"

#' toydata
#'
#' Toy dataset containing five omics matrices that can be used for testing purposes.
#'
#' @format A list of five matrices,  one for each omics type. Each matrix contains 45 rows corresponding
#' to patient samples. The genes and gene products are in the columns.
#' \itemize{
#' \item M mutations, binary, 20 columns
#' \item CN copy number changes, ordinal, 20 columns
#' \item T transcriptome, continuous (Gaussian), 20 columns
#' \item P proteome, continuous (Gaussian), 15 columns
#' \item PP phospho-proteome, continuous (Gaussian), 20 columns
#' }
#'
"toydata"


#' simclusters
#'
#'Vector containing true cluster assigments for data in the dataset "simdata"
#'
#' @format a vector of 90 integers
#'
"simclusters"

#' simdata
#'
#'A list of two matrices containing simulated mutations and transcriptome data (normalized, transformed) for three clusters. The generative
#'model is the mixture of Bayesian networks (linear Gaussian model). The networks are stored in the dataset 'simdags'. Ground truth cluster assigments are
#'stored in the dataset 'simclusters'
#'
#' @format a list of two matrices: 'M', 90 rows (samples) and 20 columns (mutations), 'T' 90 rows and 100 columns (gene expression)
#'
"simdata"

#' simdags
#'
#'A list of three matrices representing adjacency matrices of DAGs used to generate the simulated dataset
#''simdata'. Each DAG consists of 20 binary (mutations) and 50 continuous nodes (Gaussian).
#'
#' @format a list of three binary matrices, each of size 70x70
#'
"simdags"

#' simint
#'
#'A list of interactions derived from the networks stored in 'simdags' that were used to generate
#'the dataset 'simdata'
#'
#' @format a data frame with two columns "gene1" and "gene2" and 73 rows
#'
"simint"

#' bnres2
#'
#'An object of class 'bnclustOmics' containing the results of one run of the function
#''bnclustOmics' with the parameter k=2. The object contains membership assignments, estimated MAP
#'graphs representing clusters as well as posterior probabilities of all edges for each cluster.
#'
#' @format An object of class 'bnclustOmics'
#'
"bnres2"

#' bnres3
#'
#'An object of class 'bnclustOmics' containing the results of one run of the function
#''bnclustOmics' with the parameter k=3. The object contains membership assignments, estimated MAP
#'graphs representing clusters as well as posterior probabilities of all edges for each cluster.
#'
#' @format An object of class 'bnclustOmics'
#'
"bnres3"

#' bnres3
#'
#'An object of class 'bnclustOmics' containing the results of one run of the function
#''bnclustOmics' with the parameter k=4. The object contains membership assignments, estimated MAP
#'graphs representing clusters as well as posterior probabilities of all edges for each cluster.
#'
#' @format An object of class 'bnclustOmics'
#'
"bnres4"
