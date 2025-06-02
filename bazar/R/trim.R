#' @title
#' Removes extra whitespaces from a string
#'
#' @description
#' The function \code{trim} removes unnecessary whitespaces
#' from a character vector.
#'
#' @param x
#' character. The character vector at stake.
#'
#' @return
#' A character vector of the same length as \code{x}.
#'
#' @seealso \code{\link{gsub}}.
#'
#' @export
#'
#' @examples
#' trim(c(" a b", "Hello  World "))
#'
trim <-
function (x)
{
  gsub("^\\s+|\\s+$", "", x)
}
