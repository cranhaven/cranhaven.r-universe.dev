#' @title Retrieve common significant pathways from multiple pathway analysis results
#' @description Query a list of common significant pathways from multiple pathway analysis results.
#' @param PAResults A list of data frames with the results of pathway analysis.
#' @param pThreshold The p-value threshold to determine if a pathway is enriched or significant.
#' @param useFDR Use the FDR adjusted p-value instead of the nominal p-value.
#' @return A data frame contains pathway ID and pathway names.
#' @examples
#' \donttest{
#' 
#' library(RCPA)
#'
#'
#' affyFgseaResult <- loadData("affyFgseaResult")
#' agilFgseaResult <- loadData("agilFgseaResult")
#' RNASeqFgseaResult <- loadData("RNASeqFgseaResult")
#'
#' PAResults <- list(
#'     "Affymetrix - GSE5281" = affyFgseaResult,
#'     "Agilent - GSE61196" = agilFgseaResult,
#'     "RNASeq - GSE153873" = RNASeqFgseaResult
#' )
#' 
#' commonPathways <- RCPA::getCommonPathways(PAResults)
#' 
#' print(head(commonPathways))
#' 
#' }
#'
#' @importFrom dplyr %>% filter
#' @importFrom scales trans_new
#' @export
getCommonPathways <- function(PAResults, pThreshold = 0.05, useFDR = TRUE) {
  
  if (length(PAResults) < 2) {
    stop("The number of results must be at least 2.")
  }
  
  for (PARes in PAResults) {
    if (useFDR && !("pFDR" %in% colnames(PARes))) {
      stop("The FDR adjusted p-value column is not in the results data frame.")
    } else {
      if (!("p.value" %in% colnames(PARes))) {
        stop("The p.value column is not in the results data frame.")
      }
    }
  }
  
  plotDat <- lapply(PAResults, function(DERes) {
    data.frame(DERes) %>%
      filter(
        (
          if (useFDR) {
            .data$pFDR < pThreshold
          } else {
            .data$p.value < pThreshold
          }
        )
      ) %>%
      `[[`("ID")
  })
  
  commonPathways <- Reduce(f = intersect, plotDat)
  
  allPathNames <- PAResults[[1]]$name
  names(allPathNames) <- PAResults[[1]]$ID
  labelsToList <- allPathNames[commonPathways]
  
  data.frame(ID = commonPathways, Name = labelsToList, stringsAsFactors = FALSE, row.names = NULL)
}