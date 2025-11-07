#' @title Replaces NA-values in \code{vec} with last non-NA value
#' @description Assumes first element is non-NA
#' @param vec Vector of any type
#' @note Can be replaced by \code{link[zoo]{na.locf0}}
#' @author Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
#' @export
#' @examples
#' naReplace(c(1, 2, 3, NA, NA, 4))
#' naReplace(c("text", NA, NA))
naReplace <- function(vec){
  naIds <- which(!is.na(vec))
  tab <- data.table("vec"=vec,"notNa"=0)
  tab[,"notNa" := cumsum(!is.na(vec))]
  tab[,"filledVec" := vec[1],by="notNa"]
  vec <- tab$filledVec
  return(vec)
}
