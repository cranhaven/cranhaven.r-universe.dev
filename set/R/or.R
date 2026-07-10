#' @title Get Union Set for Sets
#' @description Get union set for sets.
#' @param ... one or more objects
#' @name or
#' @return union elements
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' or(A, B)
#' or(A, B, C)
or <- function(...){
    x<-c(...)
    unique(x)
}
#' @param a one object
#'
#' @param b the other object
#' @rdname or
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %or% B
#' A %and% B %or% C

"%or%" <- function(a,b){
    unique(c(a,b))
}
