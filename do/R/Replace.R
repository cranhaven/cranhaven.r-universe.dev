#' Replace
#' @description There are two methods in this function. You can use repalce many objects to one by form and to. pattern can be used to one object replaced by the other one.
#' @param data can be number, strings, verctors, dataframe or matrix.
#' @param from replaced stings
#' @param to replacements
#' @param pattern like from:to
#' @param ignore.case logical, whether to ignore case
#' @return replaced data
#' @export
#'
#' @examples
#' Replace(data = 232,from = 2,to = 1)
#' Replace(data = c(232,'a4b'),
#'         from = c(2,'.*4'),to = 1,
#'         pattern = c('a:e','b:h'))
#' df = data.frame(
#'   a = c(232, 452),
#'   b = c("nba", "cba")
#' )
#' Replace(data = df,
#'         from = 2,to = 1,
#'         pattern = c('a:e','b:h'))
#' 
Replace <- function(data,from,to,pattern,ignore.case=FALSE){
  
  if (all(!missing(from),!missing(to))){
    for (i in 1:length(from)) {
      fromi <- from[i]
      data=Replace1(data,fromi,to,ignore.case=ignore.case)
    }
  }
  if (!missing(pattern)){
    for (j in 1:length(pattern)) {
      from=gsub(":.*","",pattern[j],ignore.case=ignore.case)
      to=gsub(".*:","",pattern[j],ignore.case=ignore.case)
      data=Replace1(data,from,to,ignore.case=ignore.case)
    }
  }
data
}

Replace1<-function(data,from,to,ignore.case=FALSE){
  if (any(is.data.frame(data),is.matrix(data))){
    for (i in 1:ncol(data)) {
      data[,i]=gsub(from,to,data[,i],ignore.case = ignore.case)
    }
  }else{
    data=gsub(from,to,data,ignore.case=ignore.case)
  }
  data
}