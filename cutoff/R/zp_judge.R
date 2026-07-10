#' Whether the Data Is Arranged from Small to Large
#'
#' @param x numeric vector
#'
#' @return logical
#' @export
#'
#' @examples
#' judge_123(c(1,2,3,4,5))
#' judge_123(c(1,3,2))
judge_123 <- function(x){
    x=x[!is.na(x)]
    if (length(x)==1) return(TRUE)
    for (i in 1:(length(x)-1)) {
        if (x[i] > x[i+1]) return(FALSE)
    }
    return(TRUE)
}
#' Whether the Data Is Arranged from Large to Small
#'
#' @param x numeric vector
#'
#' @return logical
#' @export
#'
#' @examples
#' judge_321(c(5,4,3,2,1))
#' judge_321(c(3,1,2))
judge_321 <- function(x){
    x=x[!is.na(x)]
    if (length(x)==1) return(TRUE)
    for (i in 1:(length(x)-1)) {
        if (x[i] < x[i+1]) return(FALSE)
    }
    return(TRUE)
}
