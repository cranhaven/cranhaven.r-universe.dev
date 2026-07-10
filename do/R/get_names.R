#' Get Names of Object
#' @description Return the names of input. For example: if you input a, you will get 'a'.
#' @param ... any type of data object
#'
#' @return names of object
#' @export
#'
#' @examples
#' a=c(1,2,3)
#' get_names(a,mtcars)
get_names <- function(...) {
    as.character(eval(substitute(alist(...))))
}
