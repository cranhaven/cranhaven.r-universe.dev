#' @title Get common significant DE genes from multiple DE Analysis results
#' @description Get a list of common significant DE genes from multiple DE Analysis results.
#' @param DEResults A list of data frames with the results of DE analysis.
#' @param pThreshold The p-value threshold to determine if a gene is differentially expressed.
#' @param useFDR Use the FDR adjusted p-value instead of the nominal p-value.
#' @param stat The additional statistics column to use for filtering differentially expressed genes.
#' @param statThreshold The absolute value of the statistic threshold to use for filtering differentially expressed genes.
#' Default is 0, which means no filtering.
#' @return A data frame wtih three columns: ID (Entrez IDs), Symbol and Description
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#'
#' affyDEExperiment <- loadData("affyDEExperiment")
#' agilDEExperiment <- loadData("agilDEExperiment")
#' RNASeqDEExperiment <- loadData("RNASeqDEExperiment")
#'
#' DEResults <- list(
#'     "Affymetrix - GSE5281" = rowData(affyDEExperiment),
#'     "Agilent - GSE61196"   = rowData(agilDEExperiment),
#'     "RNASeq - GSE153873"   = rowData(RNASeqDEExperiment)
#' )
#' 
#' commonDEGenes <- RCPA::getCommonDEGenes(DEResults)
#' 
#' print(head(commonDEGenes))
#'}
#' @importFrom dplyr %>% filter
#' @importFrom scales trans_new
#' @export
getCommonDEGenes <- function(DEResults, pThreshold = 0.05, useFDR = TRUE, stat = "logFC", statThreshold = 0) {
  
  if (length(DEResults) < 2) {
    stop("The number of DE results must be at least 2.")
  }
  
  for (DERes in DEResults) {
    if (useFDR && !("pFDR" %in% colnames(DERes))) {
      stop("The FDR adjusted p-value column is not in the results data frame.")
    } else {
      if (!("p.value" %in% colnames(DERes))) {
        stop("The p.value column is not in the results data frame.")
      }
    }
    
    if (!stat %in% colnames(DERes)) {
      stop("The statistic column is not in the results data frame.")
    }
  }
  
  plotDat <- lapply(DEResults, function(DERes) {
    data.frame(DERes) %>%
      filter(
        abs(.data[[stat]]) > statThreshold & (
          if (useFDR) {
            .data$pFDR < pThreshold
          } else {
            .data$p.value < pThreshold
          }
        )
      ) %>%
      `[[`("ID")
  })
  
  commonGenes <- Reduce(f = intersect, plotDat)
  
  res <- getEntrezAnnotation(commonGenes)[,c("ID", "Symbol", "Description")]
  rownames(res) <- NULL
  res
  
}

