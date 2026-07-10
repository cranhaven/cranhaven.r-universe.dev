#' Reverse String Order
#'
#' @param x can be number, strings, verctors
#'
#' @return reversed string
#' @export
#'
#' @examples 
#' reverse(123)
#' reverse(c(123,'abc'))
#' 
reverse <- function(x){
    x=as.character(x)
    x=sapply(x,function(i) paste0(rev(strsplit(i,'')[[1]]),collapse = ''))
    names(x)=NULL
    x
}
