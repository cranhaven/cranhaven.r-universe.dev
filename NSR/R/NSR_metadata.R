#'Get NSR metadata
#'
#'Returns metadata on NSR including version and citation information
#' @param bibtex_file Optional output file for writing bibtex citations.
#' @param ... Additional arguments passed to internal functions.
#' @return List containing: (1) bibtex-formatted citation information, (2) information about NSR data sources, and (3) NSR version information.
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please remember to cite both the sources and the NSR, as the NSR couldn't exist without these sources!
#' @note This function is a wrapper that returns the output of the functions NSR_citations, NSR_sources, and NSR_version.
#' @export
#' @examples {
#' metadata <- NSR_metadata()
#' }
#' 
NSR_metadata <- function(bibtex_file=NULL, ...){
  
  output <- list()
  
  output[[1]] <- NSR_citations(...)
  output[[2]] <- NSR_sources(...)
  output[[3]] <- NSR_version(...)
  
  names(output)<-c("citations","version","sources")
  
  #Write the bibtex information if a file is specified
  if(!is.null(bibtex_file)){writeLines(text = output$citations$citation, con = bibtex_file)}
  
  return(output)
  
}
