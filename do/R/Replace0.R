#' Replaced by Empty
#' 
#' @param data can be number, strings, verctors, dataframe or matrix.
#' @param ... replaced stings
#'
#' @return replaced data
#' @export
#'
#' @examples
#' Replace0(data = 232,2)
#' Replace0(data = c(232,'a4b'),2,'.*4')
#' 
#' df = data.frame(
#'   a = c(232, 452),
#'   b = c("nba", "cba")
#' )
#' Replace0(data = df, 2,'a')
#' 
Replace0 <- function(data,...){
  from <- c(...)
  Replace1<-function(data,from,to){
    if (any(is.data.frame(data),is.matrix(data))){
      for (i in 1:ncol(data)) {
        data[,i]=gsub(from,to,data[,i])
      }
    }else{
      data=gsub(from,to,data)
    }
    data
  }
    for (i in 1:length(from)) {
      data=Replace1(data,from[i],to="")
    }
  data
}