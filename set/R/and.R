#' @title Get Intersection Set for Sets
#' @description Get intersection set for sets.
#' @param ... one or more objects
#' @name and
#' @return intersection elements
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' and(A, B)
#' and(A, B, C)
and <- function(...){
    x<-list(...)
    if (length(x)==1) return(x)
    res=x[[1]]
    for (i in 2:length(x)){
        res=res[res %in% x[[i]]]
    }
    return(res)
}


#' @param a one object
#'
#' @param b the other object
#'
#' @export
#' @rdname and
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %and% B
#' A %and% B %and% C

"%and%" <- function(a,b){
    a[a %in% b]
}
