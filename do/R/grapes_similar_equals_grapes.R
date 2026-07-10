#' Locate Similarly by grep()
#'
#' @param a vector for matching
#' @param b vector for searching
#'
#' @return A list contains location information.
#' @export
#'
#' @examples 
#' 1 %s=% c(1,12,3)
#' c(1,2) %s=% c(1,12,3)
"%s=%" <- function(a,b){
    loc=list()
    for (i in 1:length(a)) {
        loc=c(loc,list(grep(a[i],b)))
        names(loc)[i]=a[i]
    }
    loc
}


