#' @title Get Entrez annotation
#' @description This function retrieves Entrez annotation data from NCBI.
#' @param entrezIds A vector of Entrez IDs.
#' @return A data frame with Entrez annotation or NULL if retrieval fails. 
#' The columns are ID (Entrez ID), Symbol, Description, OtherDesignations, OtherAliases, and Chromosome.
#' @examples
#' 
#' library(RCPA)
#' geneAnno <- getEntrezAnnotation(c("77267466", "77267467"))
#' 
#' @importFrom httr POST content
#' @importFrom dplyr %>%
#' @export
getEntrezAnnotation <- function(entrezIds) {
  
  # Check for required package and exit gracefully if missing
  if (!requireNamespace("XML", quietly = TRUE)) {
    warning("The XML package is not available. Please install it to use this function.")
    return(NULL)
  }
  
  # Set URL and query parameters for NCBI Entrez
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  query <- list(
    db = "gene",
    id = unique(as.character(entrezIds)) %>% paste0(collapse = ",")
  )
  
  # Attempt to retrieve data, with graceful failure if unsuccessful
  xml <- tryCatch({
    res <- POST(url, query = query) %>% content(as = "text")
    XML::xmlParse(res)
  }, error = function(e) {
    warning("Data from NCBI could not be retrieved. Please check your internet connection or try again later.")
    return(NULL)
  })
  
  # If XML retrieval failed, return NULL
  if (is.null(xml)) {
    return(NULL)
  }
  
  # Parse XML and extract data
  xpath <- "//DocumentSummary"
  results <- XML::xpathApply(xml, xpath, function(xmlDoc) {
    listDat <- XML::xmlToList(xmlDoc)
    data.frame(
      ID = listDat$.attrs[["uid"]],
      Symbol = ifelse(is.null(listDat$Name), NA, listDat$Name),
      Description = ifelse(is.null(listDat$Description), NA, listDat$Description),
      OtherDesignations = ifelse(is.null(listDat$OtherDesignations), NA, listDat$OtherDesignations),
      OtherAliases = ifelse(is.null(listDat$OtherAliases), NA, listDat$OtherAliases),
      Chromosome = ifelse(is.null(listDat$Chromosome), NA, listDat$Chromosome),
      stringsAsFactors = FALSE
    )
  }) %>%
    do.call(what = rbind) %>%
    `rownames<-`(.$ID)
  
  return(results)
}
