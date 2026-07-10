#' Concatenate vectors after converting to character.
#'
#' @param a one R objects, to be converted to character vectors.
#' @param b one R objects, to be converted to character vectors.
#'
#' @return one vector
#' @export
#'
#' @examples
#' 1 %+% 1
"%+%" <- function(a,b){
    paste0(a,b)
}

#' glue
#'
#' @export
#'
insertglue <- function(){
    rstudioapi::insertText(' %+% ')
}
