#' @title  Read the text to data.frame
#' @author Chai
#' @description Read the strings and transform to
#' the data.frame.
#' @importFrom  utils read.table
#' @param text strings
#' @param header logical value
#' @param ... for more see \code{\link[utils]{read.table}}
#' @return  A data.frame
#' @export
read.txt <- function(text, header = TRUE, ...) {
  return(read.table(text = text, header = header, ...))
}
