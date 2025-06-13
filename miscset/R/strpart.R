#' @name str_part
#' @keywords extract part string
#' @author Sven E. Templer
#' @title Split String and Return Part
#' @description 
#' Return the \code{n}th part of a splitted string.
#' @param x Character vector.
#' @param split Regular expression splitting strings.
#' @param n Number of part to extract.
#' @param ... Arguments passed to \code{strsplit}.
#' @param roll Logical, if to use the last when less than maximum parts.
#' @return
#' A character vector of the extracted parts.
#' @seealso
#' \link{strsplit}
#' @examples
#' #
#' 
#' s <- c("abc","abcd","abc")
#' 
#' str_part(s, "", 4)
#' str_part(s, "", 4, roll=TRUE)
#' 
#' #

#' @export str_part
str_part <- function (x, split, n, ..., roll = F) {
  x <- strsplit(x, split, ...)
  if (roll)
    x <- unlist(lapply(x, function (y) tail(y, 1)))
  else
    x <- unlist(lapply(x, function (y) y[n]))
  return(x)
}

#' @rdname str_part
#' @export strpart
strpart <- function (x, split, n, ..., roll = F) {
  .Deprecated('str_part')
  x <- strsplit(x, split, ...)
  if (roll)
    x <- unlist(lapply(x, function (y) tail(y, 1)))
  else
    x <- unlist(lapply(x, function (y) y[n]))
  return(x)
}
