#' Select character from last
#'
#' @param x vector
#' @param n If missing, the last element will be used.
#' 
#'
#' @return last element
#' @export
#'
#' @examples
#' letters |> last()
#' letters |> last(1:2)
last <- function(x,n){
    if (missing(n)){
        x[length(x)]
    }else{
        rev(x)[n] |> rev()
    }
}




#' Select dataframe row from last
#'
#' @param x dataframe
#' @param n If missing, the last element will be used.
#' 
#'
#' @return last row
#' @export
#'
#' @examples
#' mtcars |> last_row()
#' mtcars |> last_row(1:2)
last_row <- function(x,n){
    if (missing(n)){
        x[nrow(x),]
    }else{
        rown <- rev(seq(1:nrow(x)))[n]
        x[rown,]
    }
}

#' Select dataframe column from last
#'
#' @param x dataframe
#' @param n If missing, the last element will be used.
#' 
#'
#' @return last column
#' @export
#'
#' @examples
#' mtcars |> last_column()
#' mtcars |> last_column(1:2)
last_column <- function(x,n){
    if (missing(n)){
        x[,ncol(x)]
    }else{
        rev(x)[,n] |> rev()
    }
}