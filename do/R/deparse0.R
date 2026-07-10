#' substitue, deparse and paste
#'
#' @param x one object
#'
#' @return character
#' @export
#'
#' @examples
#' \donttest{
#' deparse0(j)
#' }
#' 
deparse0 <- function(x){
    paste0(deparse(substitute(x)),collapse = '')
}