#' @title List object sizes and types
#'
#' @description
#' \code{lso} lists object sizes and types.
#'
#' @details
#' This function list the sizes and types of all objects in an environment.
#' By default, the list describes the objects in the current environment, presented in descending order
#' by object size and reported in megabytes (Mb).
#'
#' @param pos a number specifying the environment as a position in the search list.
#' @param pattern an optional \link{regular expression}. Only names matching pattern are returned. \link{glob2rx} can be used to convert wildcard patterns to regular expressions.
#' @param order.by column to sort the list by. Values are \code{"Type"}, \code{"Size"}, \code{"Rows"}, and \code{"Columns"}.
#' @param decreasing logical. If \code{FALSE}, the list is sorted in ascending order.
#' @param head logical. Should output be limited to \code{n} lines?
#' @param n if \code{head=TRUE}, number of rows should be displayed?
#' @export
#' @importFrom utils object.size
#' @return a data.frame with four columns (Type, Size, Rows, Columns) and object names as row names.
#' @author
#' Based on based on postings by Petr Pikal and David Hinds to the r-help list in 2004 and
#' modified Dirk Eddelbuettel, Patrick McCann, and Rob Kabacoff.
#' @references \url{https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session/}.
#' @examples
#' data(cardata)
#' data(cars74)
#' lso()
lso <- function (pos = 1, pattern, order.by = "Size",
                        decreasing=TRUE, head=TRUE, n = 10) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  if (length(names) > 0) {
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size) / 10^6 # megabytes
    obj.dim <- t(napply(names, function(x)
      as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
    names(out)[2] <- "Size_Mb"
    if (head)
      out <- head(out, n)
    out
  } else {
    cat("No objects found.\n")
  }
}
