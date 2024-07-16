#' @title taxo_fuzzy_match
#' @description Fuzzy matching with names
#' @param name Name to search
#' @param master List of names
#' @param dist Distance tolerance, Default: 2
#' @return Matched name, string distance and original name. Null if not found.
#' @details Fuzzy matching with names in the master list and return best match.
#' @importFrom stringdist stringdist
#' @family Name functions
#' @examples
#' \donttest{
#'master <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Acodon hirtus",
#'                                     "Akodon longipilis apta",
#'                                     "Akodon longipilis castaneus",
#'                                     "Chroeomys jelskii",
#'                                     "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = FALSE)
#'  taxo_fuzzy_match("Acodon hirta",master)
#' }
#' @rdname taxo_fuzzy_match
#' @export
taxo_fuzzy_match <- function(name,master,dist=2){
  ret <- master[agrep(name,master$canonical),c("canonical")]

  if(identical(ret, character(0)) ){
    ret <- NULL
  } else {
    ret <- data.frame("canonical"=ret,
                      stringsAsFactors = F)
    ret$dist <- stringdist(name,ret$canonical)
    ret$sname <- replicate(nrow(ret), name)
    if(min(ret$dist)>dist){
      ret <- NULL
    } else {
      ret <- ret[which(ret$dist==min(ret$dist)),]
    }
  }
  return(ret)
}
