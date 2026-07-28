#'Get metadata on current NSR version
#'
#'Return metadata about the current NSR version
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing current NSR version number, build date, and code version.
#' @export
#' @examples{
#' NSR_version_metadata <- NSR_version()
#' }
#'
NSR_version <- function(...){
  

  return(nsr_core(mode = "meta",...)$meta)

}#NSR version
