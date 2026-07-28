#'Get information on sources used by the NSR
#'
#'Return metadata about the current NSR sources
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing information about the sources used in the current NSR version.
#' @export
#' @examples {
#' sources <- NSR_sources()
#' }
#' 
NSR_sources <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  return(nsr_core(mode = "sources",...)$sources)

}#NSR sources
