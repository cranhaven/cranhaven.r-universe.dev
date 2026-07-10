#' Concatenate Strings
#' @description Concatenate vectors by adding a symbol.
#' @param x vectors
#' @param symbol defulat is '+'
#'
#' @return a concatenated string
#' @export
#'
#' @examples
#' inner_Add_Symbol(c('a','b'))
#' inner_Add_Symbol(c('a','b'),"$")
#' inner_Add_Symbol(c('a','b'),"")
inner_Add_Symbol <- function(x,symbol="+"){
  paste0(x,collapse = symbol)
}
