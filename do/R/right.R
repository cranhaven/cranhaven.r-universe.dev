#' Truncate Characters from the Right
#'
#' @param x can be number, strings, verctors, dataframe or matrix.
#' @param n length
#'
#' @return substring
#' @export
#'
#' @examples
#' right("abcd",3)
#' right(c("abc","gjh"),2)
#' df = data.frame(
#'   a = c(123,234,456),
#'   b = c("abc","bcd","hjg")
#' )
#' right(df,2)
right <- function(x, n){
  if (any(is.data.frame(x),is.matrix(x))){
    for (i in 1:ncol(x)) {
      x.i=as.character(x[,i])
      x[,i]=substr(x.i, nchar(x.i)-n+1, nchar(x.i))
    }
    x
  }else if(is.factor(x)){
    x.i=as.character(x)
    substr(x.i, nchar(x.i)-n+1, nchar(x.i))
  }else{
    substr(x, nchar(x)-n+1, nchar(x))
  }
}

