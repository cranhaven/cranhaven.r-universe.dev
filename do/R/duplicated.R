#' Determine All Duplicate Elements
#'
#' @param x character
#'
#' @return logical value
#' @export
#'
#' @examples
#' x=c(1,3,2,1,2)
#' duplicated(x)
#' duplicated_all(x)
duplicated_all <- function(x){
    x %in% x[duplicated(x)]
}
#' Determine Duplicate Elements in the Last Position
#'
#' @param x character
#'
#' @return logical value
#' @export
#'
#' @examples
#' x=c(1,3,2,1,2)
#' duplicated(x)
#' duplicated_last(x)
duplicated_last <- function(x){
    rev(duplicated(rev(x)))
}