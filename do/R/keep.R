#' Keep objects 
#'
#' @param ... one or more objects
#' @param envir environment, default is global
#'
#' @export
#'
#' @examples
#' \donttest{
#' a <- 1
#' b <- 2
#' d <- 4
#' keep(a)
#' }
keep <- function(..., envir = .GlobalEnv){
    x <- get_names(...)
    lst <- ls(envir = envir)[ ! ls(envir = envir) %in% x]
    rm(list = lst, envir = envir)
}