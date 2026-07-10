#' Remove elements by number of characters
#'
#' @param x one vector
#' @param least least number of characters
#' @param most most number of characters 
#'
#' @return removed vector
#' @export
#'
#' @examples
#' x <- c('a','abc','abcd',NA)
#' rm_nchar(x,least = 1)
#' rm_nchar(x,most = 4)
#' rm_nchar(x,least = 1, most = 4)
rm_nchar <- function(x,least,most){
    if (!missing(least)) x <- x[nchar(x) > least]
    if (!missing(most)) x <- x[nchar(x) < most]
    x
}