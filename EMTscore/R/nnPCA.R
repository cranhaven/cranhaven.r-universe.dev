############################# Execution of nnPCA function

## Function Input:  single-cell gene expression data, all gene sets in a list, and k represents the index for a gene set in a list

## Function Output: Scores for each cell/sample ID along with pathwayName
#' nnPCA methods to calculate EMT score
#' 
#' @param geneExp A numeric matrix of gene expression values where rows represent genes and columns represent samples/cells.
#' @param geneList A list of signature gene sets. It should contain a column named `GeneName` which lists the genes to be used in the analysis.
#' @param dimension An integer specifying the number of principal components to compute (default is 1).
#' @param score_names A character vector specifying the names to assign to the columns of the returned data frame containing the scores (e.g., c('M1_score', 'M2_score')).
#'
#' @return A data frame containing sample/cell IDs and their respective EMT scores.
#' @export
#'
#' @examples
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' library(nsprcomp)
#' Execute_nnPCA(geneExp, Panchy_et_al_E_signature, dimension=1, score_names='E_score')
#' nnPCA_Mscore <- Execute_nnPCA(
#' geneExp, 
#' Panchy_et_al_E_signature, 
#' dimension=2,
#' score_names=c('M1_score','M2_score')
#' )

Execute_nnPCA <- function(geneExp, geneList, dimension, score_names)
{
    geneExp <- geneExp[, colnames(geneExp) %in% geneList$GeneName]
    pc_feature <- nsprcomp(as.matrix(geneExp), nneg=TRUE, ncomp=dimension)
    dfpc <- data.frame(pc_feature$x)
    dfpc[is.na(dfpc)] <- 0
    colnames(dfpc) <- score_names
    return(dfpc)
}
