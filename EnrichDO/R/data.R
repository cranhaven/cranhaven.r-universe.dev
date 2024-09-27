#' Detailed annotation information for 4831 DO terms.
#'
#' A dataset includes 4831 DO terms of hierarchical information, annotated gene information, and weight information
#'
#'
#' @format A data frame with 4813 rows and 10 variables:
#' \describe{
#'   \item{DOID}{the DOterm ID on enrichment}
#'   \item{level}{the hierarchy of the DOterm in the DAG graph}
#'   \item{gene.arr}{all genes related to the DOterm}
#'   \item{weight.arr}{gene weights in each node}
#'   \item{parent.arr}{the parent node of the DOterm}
#'   \item{parent.len}{the number of parent.arr}
#'   \item{child.arr}{child nodes of the DOterm}
#'   \item{child.len}{the number of child.arr}
#'   \item{gene.len}{the number of all genes related to the DOterm}
#'   \item{DOTerm}{the standard name of the DOterm}
#' }
"doterms"

#' All DO term annotated genes.
#'
#' A dataset includes 15106 genes.
#'
#'
#' @format An character array with 15106 elements:
"dotermgenes"

