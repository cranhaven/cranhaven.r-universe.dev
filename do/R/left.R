#' Truncate Characters from the Left
#' 
#' @param x can be number, strings, verctors, dataframe or matrix.
#' @param n length
#' @return substring
#' @export
#'
#' @examples
#' left("abcd",3)
#' left(c("abc","gjh"),2)
#' df = data.frame(
#'   a = c(123,234,456),
#'   b = c("abc","bcd","hjg")
#' )
#' left(df,2)
left <- function(x, n){
  if (any(is.data.frame(x),is.matrix(x))){
    for (i in 1:ncol(x)) {
      x[,i]=substr(x[,i], 1, n)
    }
    x
  }else{
    substr(x, 1, n)
  }
}
