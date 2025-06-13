#' @name strextr
#' @keywords extract string
#' @author Sven E. Templer
#' @title Extract a Substring
#' @description 
#' This function extracts substring(s) which match a given pattern.
#' @param x Character vector.
#' @param pattern Regular expression.
#' @param sep Character string which separates the fields. 
#' @param mult Logical, if multiple matching fields should be returned,
#' or otherwise NA.
#' @param unlist Logical, unlists multiple results.
#' @param cores Integer for number of computational cores to use.
#' @details 
#' The function is deprecated and will be removed with \code{miscset} version 2.
#' It is recommended to use \link[stringr]{str_extract} or
#' \link[stringr]{str_extract_all} instead.
#' @return
#' A list of character vectors containing the substrings that are
#' matching \code{pattern} and are separated by \code{sep} or \code{NA} if
#' the pattern could not be found.
#' @examples
#' #
#' 
#' library(stringr)
#' 
#' s <- c("A1 B1 C1","A2 B2", "AA A1", "AA", "B1 A1", "BB AB A1")
#' 
#' strextr(s, "^[AB][[:digit:]]$") # deprecated
#' str_extract(s, "[AB][:digit:]")
#' 
#' strextr(s, "^[AB][[:digit:]]$", mult = TRUE) # deprecated
#' str_extract_all(s, "[AB][:digit:]")
#' 
#' strextr(s, "^[AB][[:digit:]]$", mult = TRUE, unlist = TRUE) # deprecated
#' unlist(str_extract_all(s, "[AB][:digit:]")) # has no <NA> values
#' 
#' strextr(s, "^[C][[:digit:]]$") # deprecated
#' str_extract(s, "[C][:digit:]")
#' 
#' #

#' @export strextr
strextr <- function (x, pattern, sep = " ", mult = F, unlist = F, cores = 1) {
  .Deprecated(msg = "  'strextr' is deprecated and will be removed with the release of miscset version 2.
  Use 'stringr::str_extract' instead.
  See examples in ?strextr")
  
    x <- strsplit(x, sep)
    x <- mclapply(x, function (y) {
      y <- grep(pattern, y, value = T)
      l <- length(y)
      if ((!mult & l>1) | l==0) y <- NA
      return(y)
    }, mc.cores = cores)
    if (!mult | unlist) x <- unlist(x)
    return(x)
}
