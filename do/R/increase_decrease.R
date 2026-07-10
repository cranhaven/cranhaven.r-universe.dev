#' increase character
#'
#' @param chr one vector
#'
#' @return increased vector
#' @export
#'
#' @examples
#' set.seed(2020)
#' x=rnorm(20)
#' increase(x)
increase <- function(chr){
    if (!is.atomic(chr)) stop('chr must be a vector')
    chr[order(chr,decreasing = FALSE)]
}
#' decrease character
#'
#' @param chr one character vector
#'
#' @return decreased vector
#' @export
#'
#' @examples
#' set.seed(2020)
#' x=rnorm(20)
#' decrease(x)
decrease <- function(chr){
    if (!is.atomic(chr)) stop('chr must be a vector')
    chr[order(chr,decreasing = TRUE)]
}