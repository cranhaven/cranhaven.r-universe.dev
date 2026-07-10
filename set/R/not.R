#' @title Get Elements only Existed in Dataset a
#' @description Get elements only existed in dataset a.
#' @param ... one or more objects
#' @name not
#' @return elements only existed in dataset a
#' @export
#'
#' @examples
#' A <- c("a","b")
#' B <- c("a","b","c","d")
#' not(B, A)
#'
#' E <- c('d')
#' not(B, A, E)
not <- function(...){
    x<-list(...)
    if (length(x)==1) return(x)
    res=x[[1]]
    for (i in 2:length(x)){
        res=res[! res %in% x[[i]] ]
    }
    return(res)
}
#' @param a one object
#'
#' @param b the other object
#' @export
#' @rdname not
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' B %not% A

"%not%" <- function(a,b){
    a[! a %in% b ]
}
