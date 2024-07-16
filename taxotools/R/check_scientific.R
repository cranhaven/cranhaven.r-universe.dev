#' Parse and resolve a scientific name string
#'
#' Parse the name using Global Names Resolver 'GNR' and Global Biodiversity
#'  Information Facility 'GBIF' parse API to make sure the name is scientific
#'  name
#'
#' @param name scientific name string to be checked
#' @return Resolved canonical name (NULL if not matched)
#'
#' @family Name functions
#' @importFrom taxize gbif_parse gnr_resolve
#' @examples
#' \donttest{
#' check_scientific("Akodon longipilis (Waterhouse, 1837)")
#' check_scientific("Mus longipilis Waterhouse, 1837")
#' check_scientific("Akodon hershkovitzi Patterson, Gallardo, and Freas, 1984")
#' }
#' @export
check_scientific <- function(name){
  res <- tryCatch({gnr_resolve(name)},
           error=function(cond) {
             message("There was an error")
             return(NULL)})  
  if(!is.null(res)){
    res1 <- tryCatch({gbif_parse(res$matched_name[1])},
                     error=function(cond) {
                       message("There was an error")
                       return(NULL)})
    return(res1$canonicalname)
  } else {
    return(NULL)
  }
}