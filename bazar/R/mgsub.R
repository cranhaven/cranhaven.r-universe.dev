#' @title
#' Multiple gsub
#'
#' @description
#' The function \code{mgsub} is a `multiple' version of \code{\link{gsub}}.
#'
#' @param pattern
#' character vector containing regular expressions 
#' to be matched in the given character vector.
#'
#' @param replacement
#' a replacement vector of the same length as \code{pattern} for matched pattern. 
#' Coerced to character if possible. 
#'
#' @param x
#' vector or NULL: the values to be matched against.
#'
#' @param ...
#' additional parameters to be passed to \code{gsub}. 
#'
#' @return
#' A character vector of the same length as \code{x}.
#'
#' @author
#' Theodore Lytras on StackOverflow, see \url{http://stackoverflow.com/a/15254254/3902976}
#' 
#' @seealso \code{\link[base]{gsub}} from package \pkg{base}.
#'
#' @export
#'
#' @examples
#' mgsub(c("aa", "AA"), c("bb", "BB"), c("XXaaccAACC", "YYaaccAACC", "ZZaaccAACC"))
#'
mgsub <-
function(pattern,
         replacement,
         x,
         ...)
{
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in seq_along(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
