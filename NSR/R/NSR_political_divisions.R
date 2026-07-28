#'Get information on political divisions with checklists within the NSR
#'
#'NSR_political_divisions returns information on political divisions with checklist information present in the NSR.
#' @param by_country Logical. If TRUE (the default), will return a data.frame of checklists for each country. If FALSE, will return a data.frame of countries for each checklist.
#' @param ... Additional parameters passed to internal functions.
#' @return data.frame containing information on political divisions within the NSR database.
#' @note Setting checklist to FALSE returns a list of political divisions that can be used to standardize spellings.
#' @export
#' @examples \dontrun{
#' 
#' #To get a list of all political divisions with comprehensive checklists:
#' checklists_per_country <- NSR_political_divisions()
#' 
#' #To get a list of all checklists the associated countries, set "by_country" to FALSE
#' countries_per_checklist <- NSR_political_divisions(by_country=FALSE)
#' 
#' }
NSR_political_divisions <- function(by_country = TRUE, ...){

  if(by_country){
    return(nsr_core(mode = "country_checklists",...)$country_checklists)
    }else{
      return(nsr_core(mode = "checklist_countries",...)$checklist_countries)
      }

  
}
