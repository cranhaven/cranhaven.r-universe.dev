#' Trim
#'
#' @param x can be vector or dataframe or matrix
#' @param pattern one or more pattern pattern
#' @name Trim
#' @return a trimed string
#' @export
#'
Trim <- function(x,pattern=' '){
    x=Trim_left(x,pattern)
    x=Trim_right(x,pattern)
    x
}
#' @rdname Trim
#' @aliases trim-left
#' @export
Trim_left <- function(x,pattern=' '){
    Trim_vector <- function(x,pattern){
        for (j in 1:length(x)) {
            x.j=x[j]
            for (i in 1:nchar(x.j)) {
                if (left(x.j,1) %in% pattern){
                    x.j=mid(x.j,2,nchar(x.j))
                }else{
                    break(i)
                }
            }
            x[j]=x.j
        }
        x
    }
    if (is.atomic(x)){
        Trim_vector(x,pattern)
    }else if (is.data.frame(x) |
              is.matrix(x)){
        for (i in 1:ncol(x)) {
            x[,i]=Trim_vector(as.character(x[,i]),pattern)
        }
        x
    }
}
#' @rdname Trim
#' @aliases trim-right
#' @export
Trim_right <- function(x,pattern=' '){
    Trim_vector <- function(x,pattern){
        x=reverse(x)
        for (j in 1:length(x)) {
            x.j=x[j]
            for (i in 1:nchar(x.j)) {
                if (left(x.j,1) %in% pattern){
                    x.j=mid(x.j,2,nchar(x.j))
                }else{
                    break(i)
                }
            }
            x[j]=reverse(x.j)
        }
        x
    }
    if (is.atomic(x)){
        Trim_vector(x,pattern)
    }else if (is.data.frame(x) |
              is.matrix(x)){
        for (i in 1:ncol(x)) {
            x[,i]=Trim_vector(as.character(x[,i]),pattern)
        }
        x
    }
}