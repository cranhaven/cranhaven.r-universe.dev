#' Locate Accurately
#'
#' @param a vector for matching
#' @param b vector for searching
#'
#' @return If length of a is one, a vector will be return. If length of a is more
#'     than one, a list for each element will be return.
#' @export
#'
#' @examples
#' a=c(1,2,3,4)
#' b=c(1,2,3,1,4,1,5,6,1,4,1)
#' a %==% b
"%==%"<- function(a,b){
  if (length(a)==1){
    (1:length(b))[a == b]
  }else if(length(a) > 1){
    for (i in 1:length(a)) {
      if (i==1){location=list()}
      location.i=(1:length(b))[a[i] == b]
      location=c(location,list(location.i))
      names(location)[length(location)]=a[i]
    }
    location
  }
}
