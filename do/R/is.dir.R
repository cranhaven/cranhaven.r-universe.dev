#' Whether file path is directory
#'
#' @param ... one or more file path
#'
#' @return logical
#' @export
#'
is.dir <- function(...){
    c(...) |> 
        file.info() |> 
        select(j='isdir',drop=TRUE)
}