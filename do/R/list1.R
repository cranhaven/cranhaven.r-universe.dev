#' Select list one
#'
#' @param x list
#'
#' @return element in list 1
#' @export
#'
#' @examples
#' x = list(mtcars)
#' x  |> list1()
list1 <- function(x){
    x[[1]]
}