#' Strips out html tags
#' 
#' @param htmlString character vector of html strings.
#' @details Internal function to strip out html tags into pure text for later processing with exported functions of rcatfish
#' @return Character vector 
#' @author Samuel R. Borstein
#' @noRd

StripFun <- function(htmlString){
  read.html <- lapply(htmlString, xml2::read_html)
  stripped <- lapply(read.html, rvest::html_text)
  StrippedResults <- unlist(stripped)
  return(StrippedResults)
}
