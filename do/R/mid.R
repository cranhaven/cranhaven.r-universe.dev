#' Truncate Characters from the Inside
#'
#' @param x can be number, strings, verctors, dataframe or matrix.
#' @param start starting position
#' @param n length, n can be less than zero
#' @return substring
#'
#' @export
#'
#' @examples
#' mid("abcd",3,1)
#' mid(c("abc","gjh"),2,2)
#' df = data.frame(
#'   a = c(123,234,456),
#'   b = c("abc","bcd","hjg")
#' )
#' mid(df,2,1)
#' mid(df,2,-2)
mid <- function(x,start,n=100000000000){
  if (n < 0){
    right(left(x,start),abs(n))
  }else if(n >0) {
    if (any(is.data.frame(x),is.matrix(x))){
      for (i in 1:ncol(x)) {
        x.i=as.character(x[,i])
        x[,i]=substr(x.i,start,start+n-1)
      }
      x
    }else{
      substr(x,start,start+n-1)
    }
  }
}

