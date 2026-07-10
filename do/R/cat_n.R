#' print vector by lines
#'
#' @param x one vector
#' @param n number if element in each line, default is 3
#' @param ind indentation, default is 0
#'
#' @return print vector by lines
#' @export
#'
#' @examples
#' cat_n(1:10)
#' cat_n(1:10,ind=3)
cat_n <- function(x,n=3,ind=0){
    for (i in 1:length(x)) {
        if (i==1) cat(paste0(rep(' ',ind),collapse = ''))
        cat(x[i],'')
        if (i %% n ==0) cat(paste0('\n',paste0(rep(' ',ind),collapse = '')))
    }
}