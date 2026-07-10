#' Number of Characters
#'
#' @param x can be number, strings, verctors, dataframe or matrix.
#'
#' @return number of characters in each location
#' @export
#'
#' @examples
#' Nchar("abcd")
#' Nchar(c("abc","gjh"))
#' df = data.frame(
#'   a = c(1,12,12.3),
#'   b = c("a","ab","abc")
#' )
#' Nchar(df)
Nchar <- function(x){
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            x[,i]=nchar(as.character(x[,i]))
        }
        return(x)
    }else{
        return(nchar(as.character(x)))
    }
}
