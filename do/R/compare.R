#' Compare two vectors
#'
#' @param a one vector
#' @param b the other vector
#' @name compare
#' @return the compared object
#' @export
#'
#' @examples
#' equal(letters,c('a','b'))
equal <- function(a,b){
    a[a %in% b]
}
#' @export
#' @rdname compare
#' @examples
#' over(1:10,5)
over <- function(a,b){
    a[a>b]
}
#' @export
#' @rdname compare
#' @examples
#' over(1:10,5)
lower <- function(a,b){
    a[a<b]
}
