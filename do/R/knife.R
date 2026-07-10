#' Knife characters
#'
#' @param x one character
#' @param n number
#' @name knife
#' @export
#'
#' @examples
#' knife_left(123,2)
#' knife_right(123,2)
knife_left <- function(x,n){
    substr(x, n+1, nchar(x))
    
}
#' @rdname knife
#' @export
knife_right <- function(x,n){
    knife_i <- function(x,n){
        x=rev(strsplit(as.character(x),'')[[1]])
        x=rev(x[-c(1:n)])
        paste0(x,collapse = '')
    }
    x1=sapply(x, function(i) knife_i(i,n))
    names(x1)=names(x)
    x1
}
