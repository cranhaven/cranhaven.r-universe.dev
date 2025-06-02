#' @title 
#' Back slang
#' 
#' @description 
#' The \code{verlan} function reverses the order of 
#' the characters in a string. 
#' 
#' @param x
#' character. A vector of strings. 
#' 
#' @return 
#' A character vector of the same length as \code{x}. 
#'
#' @export
#' 
#' @examples 
#' verlan("baba") ## "abab"
#' verlan(c("radar", "paul")) ## c("radar", "luap")
#' 
verlan <- 
function(x)
{
  vapply(strsplit(x, ""), 
         FUN = function(x) paste(rev(x), collapse = ""), 
         FUN.VALUE = character(1L))
}
