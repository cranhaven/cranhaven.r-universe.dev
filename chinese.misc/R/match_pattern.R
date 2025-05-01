#' Extract Strings by Regular Expression Quickly
#'
#' Given a pattern and a character vector, the function will extract 
#' parts of these characters that match 
#' the pattern. It is simply a wrapper of \code{\link{regmatches}}.
#'
#' @param pattern a length 1 regular expression to be matched.
#' @param where a character vector, each of its elements may or may not have parts that 
#' match the specified pattern.
#' @param vec_result should be \code{TRUE} or \code{FALSE}. If \code{TRUE} (default), 
#' all matched parts will be returned in a character vector. If \code{FALSE}, a list is returned, 
#' each element of the list represents the matching result of the corresponding element 
#' in \code{where}. If an element in \code{where} has nothing matching the pattern, the result 
#' is still an element in the list and assigned \code{character(0)}.
#' @param perl default is \code{FALSE}.  
#' Should Perl-compatible regexps be used?
#'
#' @return a character vector or a list. If an element in \code{where} is \code{NA}, the result 
#' corresponds to this element is \code{character(0)}.
#'
#' @export
#' @examples
#' p <- "x.*?y"
#' x <- c("x6yx8y", "x10yx30y", "aaaaaa", NA, "x00y")
#' y <- match_pattern(p, x)
#' y <- match_pattern(p, x, vec_result = FALSE)
match_pattern <-
function(pattern, where, vec_result = TRUE, perl = FALSE) {
  where <- whetherencode(where)
  pattern <- whetherencode(pattern)
  middle <- gregexpr(pattern, where, perl = perl)
  if (vec_result) {
    return(unlist(regmatches(where, middle)))
  }
  else {
    return(regmatches(where, middle))
  }
}
