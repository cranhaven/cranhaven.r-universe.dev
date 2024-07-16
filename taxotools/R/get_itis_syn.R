#' Get Integrated Taxonomic Information System 'ITIS' Synonyms for a Scientific
#'  Name
#'
#' Fetch Synonyms using Integrated Taxonomic Information System 'ITIS' web
#'  service
#'
#' @param scname Scientific Name
#' @return a list containing synonyms
#' @importFrom taxize get_tsn synonyms
#' @family ITIS functions
#' @examples
#' \donttest{
#' get_itis_syn("Abrothrix longipilis")
#' get_itis_syn("Abditomys latidens")
#' }
#' @export
get_itis_syn <- function(scname){
  tsn <- get_tsn(scname, rows=1)[1]
  t1 <- NULL
  if(!is.na(tsn)){
    syn <- synonyms(tsn,db="itis")
    eval(parse(text=paste("t <- (syn$'",tsn,"')",sep='')))
    if(!is.null(t)){
      eval(parse(text=paste("t <- (syn$'",tsn,"'$acc_name)",sep='')))
      eval(parse(text=paste("t1 <- (syn$'",tsn,"'$syn_name)",sep='')))
      return(unique(c(t,t1)))
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}
