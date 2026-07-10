#' Unique Without NA
#'
#' @param x vector
#'
#' @return unique values with no NA
#' @export
#'
#' @examples
#' x=c(1,2,3,1,NA)
#' unique(x)
#' unique_no.NA(x)
unique_no.NA <- function(x){
    if (!is.vector(x)) stop('x must be a vector')
    u=unique(x)
    u[!is.na(u)]
}


